package io.dylemma.api

import cats.data.EitherT
import cats.effect.{ IO, SyncIO }

trait ApiDsl {
	final implicit class IoForApiOps[A](io: IO[A]) {
		def forApi: Api[A] = Api.liftF(io)
	}

	final implicit class SyncIoForApiOps[A](sio: SyncIO[A]) {
		def forApi: Api[A] = Api.liftF(sio)
	}

	final implicit class EitherForApiOps[A](e: Either[ApiError, A]) {
		def forApi: Api[A] = Api.wrap(e)
	}

	final implicit class EitherTForApiOps[A](et: EitherT[IO, ApiError, A]) {
		def forApi: Api[A] = Api.liftE(et)
	}
}
