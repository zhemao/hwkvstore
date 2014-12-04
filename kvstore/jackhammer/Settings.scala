package jackhammer

class DefaultSettings extends HammerSettings {
  val RISCV = "/home/ff/cs250/install/riscv"
  val PROJ = "sha3"
  val root = "sha3"
  val user = "cs250"
  val projectDirectory = ".."
  val baseShared = "/scratch/" + user + "/hammer"
  val baseLocal =  "/scratch/" + user + "/hammer"
  val outputsShared = baseShared + "/" + root + "/outputs"
  val resultsShared = baseShared + "/" + root + "/results"

  val no_nscratch = true;

  val executeScripts = Map("cpp" -> (projectDirectory + "/scripts/cpp.ex"))
  val parseScripts = Map("cpp" -> (projectDirectory + "/scripts/cpp.pa"))

  val qors = List("cpp")
  val designs = List("DefaultConfig0","DefaultConfig1","DefaultConfig2","DefaultConfig3","DefaultConfig4")
  val config = Some("SampleConfig")
  val configScalaPath = Some("../src/main/scala/sha3.scala")
  val version = ".v.0.1"
}
