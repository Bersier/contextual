
name := "contextual"
version := "0.1.0"

scalaVersion := "3.8.1-RC1-bin-20251123-91351e3-NIGHTLY"
resolvers += Resolver.scalaNightlyRepository
scalacOptions ++= Seq(
  "-Wenum-comment-discard",
  "-Wimplausible-patterns",
  "-Wnonunit-statement",
  "-Wrecurse-with-default",
  "-Wsafe-init",
  "-Wtostring-interpolated",
  "-WunstableInlineAccessors",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wwrong-arrow",
  "-Xcook-docs",
  "-Xkind-projector:underscores",
  "-Ycheck-all-patmat",
  "-Ydebug-pos",
  "-Yexplicit-nulls",
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
