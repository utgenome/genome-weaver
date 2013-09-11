/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package utgenome.weaver

import sbt._
import Keys._
import xerial.sbt.Pack._

object GenomeWeaverBuild extends Build {

  val SCALA_VERSION = "2.10.2"

  def releaseResolver(v:String) : Resolver = {
    val profile = System.getProperty("profile", "default")
    profile match {
      case "default" => {
        val nexus = "https://oss.sonatype.org/"
        if(v.trim.endsWith("SNAPSHOT"))
          "snapshots" at nexus + "content/repositories/snapshots"
        else
          "releases" at nexus + "service/local/staging/deploy/maven2"
      }
      case p => {
        sys.error("unknown profile:%s".format(p))
      }
    }
  }

  lazy val buildSettings = Defaults.defaultSettings ++ Seq[Setting[_]](
    organization := "org.utgenome.weaver",
    organizationName := "University of Tokyo",
    organizationHomepage := Some(new URL("http://utgenome.org/")),
    description := "Scala library for reading and writing genomic data",
    scalaVersion := SCALA_VERSION,
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo <<= version { (v) => Some(releaseResolver(v)) },
    pomIncludeRepository := { _ => false },
    parallelExecution := true,
    parallelExecution in Test := false,
    crossPaths := false,
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-feature"),
    pomExtra := {
      <licenses>
        <license>
          <name>Apache 2</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        </license>
      </licenses>
        <scm>
          <connection>scm:git:github.com/utgenome/glens.git</connection>
          <developerConnection>scm:git:git@github.com:utgenome/glens.git</developerConnection>
          <url>github.com/utgenome/glens.git</url>
        </scm>
        <properties>
          <scala.version>{SCALA_VERSION}</scala.version>
          <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        </properties>
        <developers>
          <developer>
            <id>leo</id>
            <name>Taro L. Saito</name>
            <url>http://xerial.org/leo</url>
          </developer>
        </developers>
    }
  )


  object Dependencies {
    val scalaLib = Seq(
      "org.scala-lang" % "scala-reflect" % SCALA_VERSION
    )
    val testLib = Seq(
      "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
    )
    val xerialLib = Seq(
      "org.xerial" % "xerial-core" % "3.2.1",
      "org.xerial" % "xerial-lens" % "3.2.1",
      "org.xerial" % "larray" % "0.1.1",
      "org.xerial.snappy" % "snappy-java" % "1.1.0-M4"
    )
    val apacheCommons = Seq("org.apache.commons" % "commons-compress" % "1.4.1")
    val coreLib = Seq("org.xerial" % "jnuma" % "0.1.3")
  }

  import Dependencies._

  private val dependentScope = "test->test;compile->compile"


  lazy val gwCore = Project(
    id = "gw-core",
    base = file("."),
    settings = buildSettings ++ packSettings ++
      Seq(
        packMain := Map("gw-core" -> "utgenome.weaver.core.Main"),
        libraryDependencies ++= testLib ++ apacheCommons ++ xerialLib ++ scalaLib ++ coreLib
      )
  )

}








