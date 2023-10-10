package io.dylemma.api

import org.http4s.DecodeFailure

sealed trait ApiError {
	def lift: Api[Nothing] = Api.error(this)
}

case class MissingHeader(name: String) extends ApiError
case class DecodeFailureError(decoderFailure: DecodeFailure) extends ApiError
case class NotAuthenticated(attemptedAction: String) extends ApiError
case class NotAuthorized(permission: String) extends ApiError