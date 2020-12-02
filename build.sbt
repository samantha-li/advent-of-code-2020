name := "advent-of-code-2020"

scalaVersion := "2.13.4"

lazy val root = Project("advent-of-code-2020", file("."))
  .aggregate(puzzle2)

lazy val puzzle2 = project.in(file("puzzle2"))