scalaVersion := "2.13.1"

lazy val `cats-toy` = (project in file("."))
  .settings(
    name := "Cats toy"
  ) 

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.0"
libraryDependencies += "org.specs2" %% "specs2-core" % "4.8.3" % Test
libraryDependencies += "org.specs2" %% "specs2-mock" % "4.8.3" % Test
