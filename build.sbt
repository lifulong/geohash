import AssemblyKeys._ 

name := "geohash"
version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.1.7"
)

assemblySettings

mergeStrategy in assembly := {
	case m if m.toLowerCase.endsWith("manifest.mf")          => MergeStrategy.discard
	case m if m.toLowerCase.matches("meta-inf.*\\.sf$")      => MergeStrategy.discard
	case m if m.toLowerCase.startsWith("meta-inf/services/") => MergeStrategy.filterDistinctLines
	case _                                                   => MergeStrategy.first
}


