name := """shapeless-helloworld"""

version := "1.0"

scalaVersion := "2.11.7"

lazy val shapelessVersion     = "2.3.3"

libraryDependencies += "com.chuusai" %% "shapeless" % shapelessVersion


fork in run := true
