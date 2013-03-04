
scalacOptions += "-deprecation"

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.1.7")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0", sbtVersion="0.12")

resolvers += "gseitz@github" at "http://gseitz.github.com/maven/"

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.7", sbtVersion="0.12")














