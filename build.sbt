ThisBuild / scalaVersion := "2.13.10"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val core = (project in file("core")).settings(
	name := "api-monad",
	libraryDependencies ++= Seq(
		"org.typelevel" %% "cats-effect" % "3.5.2",
		"org.slf4j" % "slf4j-api" % "2.0.9",
		"org.tpolecat" %% "doobie-core" % "1.0.0-RC4",
		"co.fs2" %% "fs2-core" % "3.9.2",
		"org.http4s" %% "http4s-core" % "1.0.0-M40",
	),
	addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
)

lazy val scratch = (project in file("scratch"))
	.dependsOn(core)
	.settings(
		name := "api-monad-scratch",
		Compile / run / fork := true,
		libraryDependencies ++= Seq(
			"org.slf4j" % "slf4j-simple" % "2.0.9",
		)
	)
