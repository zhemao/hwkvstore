package jackhammer

class IclusterSettings extends HammerSettings {
  val RISCV = "/home/ff/cs250/install/riscv"
  val PROJ = "sha3"
  val root = "sha3"
  val user = System.getenv("USER")
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

  val qors = List("cpp","vcs","dc","icc")
  val designs = List("DefaultConfig0","DefaultConfig1","DefaultConfig2","DefaultConfig3","DefaultConfig4","DefaultConfig5")
  val version = ".v.1.2"
}
