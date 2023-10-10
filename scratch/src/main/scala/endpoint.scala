import io.dylemma.api.{ Api, ApiResponder }

import cats.effect.IO
import org.http4s.{ Entity, Response }

object endpoint {

	def serve: ApiResponder = for {
		_ <- Api.requirePermission("create-project")
		user <- Api.currentUser.to("create a project")
		body <- Api.requestBody.as[String]
		_ <- Api.log.info(s"$user will create project '$body'")
	} yield {
		Response[IO](
			entity = Entity.utf8String(s"New project: $body")
		)
	}
}
