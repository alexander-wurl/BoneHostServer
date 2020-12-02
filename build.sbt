name := "BoneHostServer"

version := "0.8.1.0.P"

organization := "Wurl"

scalaVersion := "2.12.6"

mainClass := Some("BoneHostServer")

resolvers += Resolver.bintrayRepo("unibas-gravis","maven")

libraryDependencies ++= Seq(
  "ch.unibas.cs.gravis" %% "scalismo" % "0.16.1",
  "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.0",
  "ch.unibas.cs.gravis" %% "scalismo-ui" % "0.13.1"
)
