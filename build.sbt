name := "algo"

scalaVersion := "2.13.12"

scalacOptions += "-deprecation"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
