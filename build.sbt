name := "algo"

scalaVersion := "2.13.15"

scalacOptions += "-deprecation"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.5" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
