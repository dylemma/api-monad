package io.dylemma

import cats.effect.IO
import org.http4s.Response

package object api {
	type ApiResponder = Api[Response[IO]]
}
