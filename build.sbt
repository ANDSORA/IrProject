name := "IrProject"

version := "1.0"

scalaVersion := "2.11.6"
    
libraryDependencies  ++= Seq(
	"org.scalanlp" %% "breeze" % "0.12",
	"org.scalanlp" %% "breeze-natives" % "0.12",
	"org.scalanlp" %% "breeze-viz" % "0.12"
)

fork := true

javaOptions += "-Xmx4g"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
