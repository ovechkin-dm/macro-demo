val macros = project.in(file("."))
  .settings(
    name := "macros",
    version := "0.1",
    scalaVersion := "2.12.7",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.7",
    addCompilerPlugin("org.scalamacros" % "paradise_2.12.7" % "2.1.0")
  )

val usage = project.in(file("usage"))
  .dependsOn(macros)
  .settings(
    name := "usage",
    version := "0.1",
    scalaVersion := "2.12.7",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.7",
    addCompilerPlugin("org.scalamacros" % "paradise_2.12.7" % "2.1.0")
  )
