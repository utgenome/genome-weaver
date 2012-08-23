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

package utgenome.glens

import sbt._
import Keys._
import com.jsuereth.pgp.sbtplugin.PgpPlugin._

object GlensBuild extends Build {

  val SCALA_VERSION = "2.9.2"

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
    organization := "org.utgenome",
    organizationName := "University of Tokyo",
    organizationHomepage := Some(new URL("http://utgenome.org/")),
    description := "Scala library for reading and writing genomic data",
    scalaVersion := SCALA_VERSION,
    //resolvers <++= version { (v) => Seq(releaseResolver(v))},
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo <<= version { (v) => Some(releaseResolver(v)) },
    publishLocalConfiguration <<= (packagedArtifacts, deliverLocal, checksums, ivyLoggingLevel) map {
      (arts, _, cs, level) => new PublishConfiguration(None, "localM2", arts, cs, level)
    },
    pomIncludeRepository := {
      _ => false
    },
    parallelExecution := true,
    crossPaths := false,
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    pomExtra := {
      <licenses>
        <license>
          <name>Apache 2</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        </license>
      </licenses>
        <scm>
          <connection>scm:git:github.com/xerial/glens.git</connection>
          <developerConnection>scm:git:git@github.com:xerial/glens.git</developerConnection>
	  <url>github.com/xerial/glens.git</url>
        </scm>
        <properties>
          <scala.version>2.9.2</scala.version>
          <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        </properties>
        <developers>
          <developer>
            <id>leo</id>
            <name>Taro L. Saito</name>
            <url>http://xerial.org/leo</url>
          </developer>
        </developers>
    },
    useGpg := true,
    useGpgAgent := false
  )


  object Dependencies {
    val testLib = Seq(
      "org.scalatest" %% "scalatest" % "2.0.M3" % "test"
    )
    val apacheCommons = "org.apache.commons" % "commons-compress" % "1.4.1"
  }

  import Dependencies._

  private val dependentScope = "test->test;compile->compile"


  lazy val glens = Project(
    id = "glens",
    base = file("."),
    settings = buildSettings ++ Seq(libraryDependencies ++= testLib :+ apacheCommons)
  ) aggregate(xerialCore, xerialLens) dependsOn(xerialCore % dependentScope, xerialLens)

  //lazy val xerial = RootProject(file("xerial"))
  lazy val xerialCore = ProjectRef(file("xerial"), "xerial-core") 
  lazy val xerialLens = ProjectRef(file("xerial"), "xerial-lens") 

}








