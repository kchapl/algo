name := "algo"

scalaVersion := "2.13.5"

scalacOptions += "-deprecation"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.9" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
