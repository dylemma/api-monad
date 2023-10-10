package io.dylemma.api

import io.dylemma.api.ApiInterpreter.{ InterpreterState, Step }

import java.sql.Connection
import scala.util.chaining._

import cats.arrow.FunctionK
import cats.data.{ Kleisli, StateT }
import cats.effect.IO
import cats.~>
import doobie.util.transactor.Transactor
import org.http4s.Request
import org.slf4j.Logger

trait ApiInterpreter {
	def run[A](op: Api[A], state: InterpreterState): IO[(InterpreterState, Either[ApiError, A])]

	def runState[A](op: Api[A]): ApiInterpreter.Step[A] = StateT { run(op, _) }
	def interpretK: Api ~> ApiInterpreter.Step = new FunctionK[Api, ApiInterpreter.Step] {
		def apply[A](op: Api[A]): Step[A] = runState(op)
	}
}

object ApiInterpreter {
	type Step[A] = StateT[IO, InterpreterState, Either[ApiError, A]]

	case class InterpreterState(
		hasAccessedBody: Boolean,
	)
	object InterpreterState {
		val init = InterpreterState(hasAccessedBody = false)
	}

	def apply(
		tx: Transactor[IO],
		auth: ApiAuth,
		logger: Logger,
		req: Request[IO],
	): ApiInterpreter = {
		new ApiInterpreterImpl(tx, auth, logger, req)
	}
}

private[api] class ApiInterpreterImpl(
	tx: Transactor[IO],
	auth: ApiAuth,
	logger: Logger,
	req: Request[IO],
) extends ApiInterpreter {

	def writeLog(op: LogStep): Unit = logger
		.atLevel(op.level)
		.pipe { l => op.cause.fold(l)(l.setCause) }
		.log(op.message)

	def run[A](op: Api[A], state: InterpreterState): IO[(InterpreterState, Either[ApiError, A])] = op match {
		case Pure(res) =>
			IO.pure(state -> res)

		case Bind(s @ DbStep(fIn), cont) =>
			// start a connection and try to run as many DbSteps as possible on it
			// before either an async boundary is encountered or a result is returned
			tx.exec.apply(Kleisli { runWithConnection(s, cont, _, state) }).flatMap {
				case (state, Left(nextStep)) => run(nextStep, state)
				case (state, Right(result)) => IO.pure(state -> result)
			}

		case Bind(start, cont) =>
			run(start, state).flatMap {
				case (state, Right(in)) => IO.defer { run(cont(in), state) }
				case (state, Left(apiError)) => IO.pure(state -> Left(apiError))
			}

		case SuspendSync(cont) =>
			IO.delay { cont() }.flatMap { run(_, state) }
		case SuspendAsync(cont) =>
			cont.flatMap { run(_, state) }

		case AuthStep(f) =>
			IO.delay { state -> Right(f(auth)) }

		case DbStep(fa) =>
			tx.trans.apply(fa).map { a => state -> Right(a) }

		case s: LogStep =>
			IO {
				writeLog(s)
				state -> Right(())
			}

		case BodyStep(f) =>
			if (state.hasAccessedBody) IO.raiseError(new IllegalStateException("Attempted to re-consume request body"))
			else IO.defer { f(req.entity) }.map { a => state.copy(hasAccessedBody = true) -> Right(a) }

		case HeadersStep() =>
			IO.pure(state -> Right(req.headers))
	}

	def runWithConnectionA[A](
		op: Api[A],
		connection: Connection,
		state: InterpreterState,
	): IO[(InterpreterState, Either[Api[A], Either[ApiError, A]])] = op match {
		case Pure(res) => IO.pure(state -> Right(res))
		case Bind(start, cont) => runWithConnection(start, cont, connection, state)
		case SuspendSync(cont) => IO.delay { cont() }.flatMap { runWithConnectionA(_, connection, state) }
		case SuspendAsync(_) => IO.pure(state -> Left(op)) // exit!
		case AuthStep(f) => IO.delay { state -> Right(Right(f(auth))) }
		case DbStep(ioA) => ioA.foldMap(tx.interpret).run(connection).map { a => state -> Right(Right(a)) }
		case s: LogStep => IO.delay { writeLog(s) }.as { state -> Right(Right(())) }
		case BodyStep(_) => IO.pure(state -> Left(op)) // exit!
		case HeadersStep() => IO.delay { state -> Right(Right(req.headers)) }
	}

	def runWithConnection[In, A](
		start: Api[In],
		cont: In => Api[A],
		connection: Connection,
		state: InterpreterState,
	): IO[(InterpreterState, Either[Api[A], Either[ApiError, A]])] = {

		start match {
			case Pure(Right(in)) =>
				IO.defer { runWithConnectionA(cont(in), connection, state) }

			case Pure(Left(err)) =>
				// `start` op returned an error, so exit the loop, ignoring `cont`
				IO.pure { state -> Right(Left(err)) }

			case Bind(fx, f) =>
				runWithConnection(fx, f, connection, state).flatMap {
					case (state2, Right(Right(in))) =>
						// inner ApiOp was completed with the connection still open, so we can
						// continue to the next step by passing its result to `cont`
						IO.defer { runWithConnectionA(cont(in), connection, state2) }

					case (state2, Right(Left(apiError))) =>
						// the `fx` op returned an error, so exit the loop, ignoring `cont`
						IO.pure(state2 -> Right(Left(apiError)))

					case (state2, Left(op)) =>
						// the `fx` op introduced an async boundary and returned a new step,
						// so exit the loop so that step can be run after closing the connection
						IO.pure(state2 -> Left(Bind(op, cont)))
				}

			case SuspendSync(resume) =>
				IO.defer { runWithConnection(resume(), cont, connection, state) }

			case SuspendAsync(_) =>
				// Async boundaries should cause the connection to close, so return
				// the step rather than running it
				IO.pure(state -> Left(Bind(start, cont))) // exit!

			case AuthStep(f) =>
				IO.defer { runWithConnectionA(cont(f(auth)), connection, state) }

			case DbStep(f) =>
				f.foldMap(tx.interpret).run(connection).flatMap { in =>
					runWithConnectionA(cont(in), connection, state)
				}

			case s: LogStep =>
				IO.delay { writeLog(s) }.flatMap { u =>
					runWithConnectionA(cont(u), connection, state)
				}

			case s @ BodyStep(_) =>
				// Consuming the request body introduces an async boundary, so we
				// should exit this loop to close the DB connection
				IO.pure(state -> Left(Bind(start, cont)))

			case HeadersStep() =>
				IO.defer { runWithConnectionA(cont(req.headers), connection, state) }
		}
	}
}
