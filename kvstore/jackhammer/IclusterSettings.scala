package jackhammer

class IclusterSettings extends HammerSettings {
  val RISCV = "/home/ff/cs250/install/riscv"
  val PROJ = "kvstore"
  val root = "kvstore"
  val user = "cs250-af"
  val projectDirectory = ".."
  val baseShared = "/scratch/" + user + "/hammer"
  val baseLocal =  "/scratch/" + user + "/hammer"
  val outputsShared = baseShared + "/" + root + "/outputs"
  val resultsShared = baseShared + "/" + root + "/results"

  val no_nscratch = true;

  val executeScripts = Map("cpp" -> (projectDirectory + "/scripts/cpp.ex"),
                           "vcs" -> (projectDirectory + "/scripts/vcs.ex"),
                           "dc" -> (projectDirectory + "/scripts/dc.ex"),
                           "icc" -> (projectDirectory + "/scripts/icc.ex"))
  val parseScripts = Map("cpp" -> (projectDirectory + "/scripts/cpp.pa"),
                          "vcs" -> (projectDirectory + "/scripts/vcs.pa"),
                          "dc" -> (projectDirectory + "/scripts/dc.pa"),
                          "icc" -> (projectDirectory + "/scripts/icc.pa"))

  val qors = List("cpp")//,"vcs","dc","icc")
  val designs = List("DefaultConfig8","DefaultConfig9","DefaultConfig10","DefaultConfig11")
  //val designs = List("DefaultConfig8","DefaultConfig9","DefaultConfig10","DefaultConfig11",
  //"DefaultConfig0","DefaultConfig2","DefaultConfig4","DefaultConfig6",
  //"DefaultConfig1","DefaultConfig3","DefaultConfig5","DefaultConfig7")//,"DefaultConfig4","DefaultConfig5")

  //val config = Some("SampleConfig")
  //val configScalaPath = Some("../src/main/scala/config/SampleConfig.scala")
  val config = None
  val configScalaPath = None

  val version = ".v.t.0"
}
