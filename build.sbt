inThisBuild(
  List(
    organization := "com.runar",
    scalaVersion := "2.12.7"
  )
)

lazy val mset = 
  RootProject(uri("git://github.com/runarorama/scala-mset.git"))

lazy val buildAll = (project in file(".")).dependsOn(mset)

name := "Maxel"
libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"
scalapropsSettings
scalapropsVersion := "0.5.5"

