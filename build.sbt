name := "algo"

scalaVersion := "2.13.13"

scalacOptions += "-deprecation"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.2" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
