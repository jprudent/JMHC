name := "majiang"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.jbehave" % "jbehave-core" % "3.7.5" % "test"

libraryDependencies += "org.jbehave" % "jbehave-scala" % "3.7.5" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M2" % "test"

libraryDependencies ++= Seq(
  "net.databinder" % "unfiltered-filter_2.10" % "0.6.7",
  "net.databinder" % "unfiltered-jetty_2.10" % "0.6.7",
  "org.clapper" % "avsl_2.10" % "1.0.1",
  "net.databinder" % "unfiltered-json_2.10" % "0.6.7",
  "net.databinder" % "unfiltered-spec_2.10" % "0.6.7" % "test"
)

resolvers ++= Seq(
  "java m2" at "http://download.java.net/maven/2"
)

publishMavenStyle := true

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))


