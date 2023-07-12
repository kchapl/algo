name := "algo"

scalaVersion := "2.13.11"

idePackagePrefix := Some("algo")

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.11" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")
