name := "majiang"

version := "1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.jbehave" % "jbehave-core" % "3.7.5" % "test"

libraryDependencies += "org.jbehave" % "jbehave-scala" % "3.7.5" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test"
