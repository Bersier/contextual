
name := "contextual"
version := "0.1.0"

scalaVersion := "3.6.0-RC1-bin-20240821-a2c53a1-NIGHTLY"
scalacOptions ++= Seq(
//  "-Werror",
  "-Wimplausible-patterns",
  "-Wnonunit-statement",
  "-Wunused:all",
  "-Ycheck-all-patmat",
  "-Ydebug-pos",
  "-Yexplicit-nulls",
  "-Yrequire-targetName",
  "-Ysafe-init-global",
  "-deprecation",
  "-experimental",
  "-explain",
  "-feature",
  "-language:strictEquality",
  "-new-syntax",
  "-release:21",
  "-source:future",
  "-unchecked",
)
