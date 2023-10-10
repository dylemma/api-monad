package io.dylemma.api

import java.util.function.Supplier

import cats.data.EitherT
import cats.effect.{ IO, SyncIO }
import cats.syntax.apply._
import cats.{ Monad, StackSafeMonad }
import doobie.ConnectionIO
import org.http4s._
import org.http4s.multipart.Part
import org.slf4j.event.Level
import org.typelevel.ci.CIString

object Api {
	def pure[A](a: A): Api[A] = Pure(Right(a))
	def error(err: ApiError): Api[Nothing] = Pure(Left(err))
	def wrap[A](result: Either[ApiError, A]): Api[A] = Pure(result)
	def delay[A](work: => A): Api[A] = SuspendSync(() => pure(work))
	def liftF[A](work: IO[A]): Api[A] = SuspendAsync(work.map(pure))
	def liftF[A](work: SyncIO[A]): Api[A] = SuspendSync(work.map(pure).unsafeRunSync _)
	def cond[A](condition: => Boolean, ifTrue: => A, errorIfFalse: => ApiError): Api[A] = SuspendSync { () => Pure(Either.cond(condition, ifTrue, errorIfFalse)) }
	def ensure(condition: => Boolean, errorIfFalse: => ApiError): Api[Unit] = cond(condition, (), errorIfFalse)
	def liftE[A](work: EitherT[IO, ApiError, A]): Api[A] = SuspendAsync(work.value.map(Pure.apply))

	object log {
		def debug(msg: => String): Api[Unit] = LogStep(Level.DEBUG, () => msg, None)
		def info(msg: => String): Api[Unit] = LogStep(Level.INFO, () => msg, None)
		def warn(msg: => String): Api[Unit] = LogStep(Level.WARN, () => msg, None)
		def warn(msg: => String, cause: Throwable): Api[Unit] = LogStep(Level.WARN, () => msg, Some(cause))
		def error(msg: => String): Api[Unit] = LogStep(Level.ERROR, () => msg, None)
		def error(msg: => String, cause: Throwable): Api[Unit] = LogStep(Level.ERROR, () => msg, Some(cause))
	}

	def requestHeaders: Api[Headers] = HeadersStep()
	object requestHeader {
		def apply[A](implicit ev: Header.Select[A], asHeader: Header[A, _]): Api[ev.F[A]] = requestHeaders.flatMap(_.get(ev) match {
			case None => error(MissingHeader(name = asHeader.name.toString))
			case Some(a) => pure(a)
		})
		def get[A](implicit ev: Header.Select[A]): Api[Option[ev.F[A]]] = requestHeaders.map(_.get(ev))

		def raw(name: String): Api[String] = requestHeaders.flatMap(_.get(CIString(name)) match {
			case None => error(MissingHeader(name))
			case Some(values) => pure(values.head.value)
		})
		def getRaw(name: String): Api[Option[String]] = requestHeaders.map(_.get(CIString(name)).map(_.head.value))
	}

	def db[A](fa: ConnectionIO[A]) = DbStep(fa)

	object requestBody {
		def entity: Api[Entity[IO]] = BodyStep(IO.pure)

		def asMedia: Api[Media[IO]] = (requestHeaders, entity).mapN(Part.apply)

		def as[A: EntityDecoder[IO, *]]: Api[A] = asMedia.flatMap { m =>
			liftE(m.attemptAs[A].leftMap(DecodeFailureError.apply))
		}
	}

	object currentUser {
		def to(attemptedAction: => String) = AuthStep(_.authenticatedUser).flatMap {
			case None => error(NotAuthenticated(attemptedAction))
			case Some(user) => pure(user)
		}
	}

	def requirePermission(permission: String) = AuthStep(_.hasPermission(permission)).flatMap {
		case true => pure(())
		case false => error(NotAuthorized(permission))
	}

	implicit val apiMonad: Monad[Api] = new StackSafeMonad[Api] {
		def pure[A](x: A) = Api.pure(x)
		def flatMap[A, B](fa: Api[A])(f: A => Api[B]) = fa.flatMap(f)
	}
}

sealed trait Api[+A] {
	def flatMap[B](f: A => Api[B]): Api[B] = Bind(this, f)
	def map[B](f: A => B): Api[B] = Bind[A, B](this, a => Pure(Right(f(a))))

	def semiflatMap[B](f: A => IO[B]): Api[B] = flatMap[B](a => Api.liftF(f(a)))
	def subflatMap[B](f: A => Either[ApiError, B]): Api[B] = flatMap(a => Api.wrap(f(a)))
	def flatMapF[B](f: A => EitherT[IO, ApiError, B]): Api[B] = flatMap(a => Api.liftE(f(a)))
}

private[api] case class Pure[A](a: Either[ApiError, A]) extends Api[A]
private[api] case class Bind[In, A](inner: Api[In], f: In => Api[A]) extends Api[A]
private[api] case class SuspendSync[A](fa: () => Api[A]) extends Api[A]
private[api] case class SuspendAsync[A](fa: IO[Api[A]]) extends Api[A]

private[api] case class AuthStep[A](f: ApiAuth => A) extends Api[A]
private[api] case class DbStep[A](fa: ConnectionIO[A]) extends Api[A]
private[api] case class LogStep(level: Level, message: Supplier[String], cause: Option[Throwable]) extends Api[Unit]
private[api] case class BodyStep[A](f: Entity[IO] => IO[A]) extends Api[A]
private[api] case class HeadersStep() extends Api[Headers]
