ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "sampleOne"
  )

libraryDependencies ++= Seq(

  // https://mvnrepository.com/artifact/org.apache.logging.log4j/log4j-api-scala
  "org.apache.logging.log4j" % "log4j-core" % "2.23.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.22.1" % Runtime,
  "org.scalatest" %% "scalatest" % "3.2.17" % "test",
  "org.scalactic" %% "scalactic" % "3.2.18"
)