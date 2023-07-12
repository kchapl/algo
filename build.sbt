name := "algo"

scalaVersion := "2.13.5"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.9" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
