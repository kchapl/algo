name := "algo"

scalaVersion := "2.13.14"

scalacOptions += "-deprecation"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.4" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
