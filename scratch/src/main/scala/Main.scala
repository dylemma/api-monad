import cats.effect.{ ExitCode, IO, IOApp }
import org.slf4j.LoggerFactory
import org.slf4j.event.Level

object Main extends IOApp {
	def run(args: List[String]): IO[ExitCode] = IO {
		val logger = LoggerFactory.getLogger(getClass)
		logger.info("Hello world")
		logger.atLevel(Level.WARN).log("Hello, warning!")
		ExitCode.Success
	}
}
