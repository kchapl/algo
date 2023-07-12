name := "algo"

scalaVersion := "2.13.11"

scalacOptions += "-deprecation"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.11" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
