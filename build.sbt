name := "99-scala"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.10.0" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
