package jackhammer

import scala.io.Source
import java.io._
//import Chisel._

import scala.util.matching.Regex
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing._
import scala.sys.process._
import Util._

trait ClusterSettings {
  val RISCV:String //global path
  val projectDirectory:String //global path
  val root:String
  val baseShared:String //global path
  val baseLocal:String //global path
  val outputsShared:String
  val resultsShared:String
  val no_nscratch:Boolean
  //val qsubSettings:Map[String,String]
  //val launchSettings:Map[String,String]
  val qsubPath = "src/main/bash/qsub.sh"
  val launchPath = "src/main/bash/launch.sh"
  val setupPath = "src/main/bash/setup.sh"
  val copyScript = "src/main/bash/copy.sh"
}

trait SourceSettings {
  val executeScripts:Map[String,String]
  val parseScripts:Map[String,String]
}

trait RunSettings {
  val qors:List[String]
  val designs:List[String]
  val version:String
}

abstract class HammerSettings extends RunSettings with SourceSettings with ClusterSettings

/*
class QOR {
  val name:String
  val executeCommand:String
  val parseCommand:String
}

class Design {
  val tag:String //hash of config?
}


class Result(parsedOutput:String) {
  val node:String
  val design:Design
  val qor:QOR
  def fromFile
  def toFile
}

class Results(storedFile:Option[String]) {
  var results:Map[Tuple2[Design,QOR],Result]
  def preview:String //pretty print
  def update(r:Result):Unit
  def toFile(file:String)
  def fromFile(file:String)
  def findOutput(d:Design,q:QOR):String //path to full result output files
  def findDesign(d:Design):String //path (including machine name) to directory which executed Design
}
*/

class OutErr(val OU:String, val ER:String) {
}

object Util {
  val whereami = System.getProperty("user.dir")
  def fixFile(f:String) = if(f.startsWith("/")) f else whereami + "/" + f
  def fixDirectory(d:String) = if(d.endsWith("/")) d else d + "/"
  def o2b(o:Option[Any]):Boolean = o match { case Some(n) => true ; case None => false}
  def execute(cmd:String,d:String=""): Tuple2[String,String] = {
    println("Executing the following command: " + cmd)
    var normalLines = new StringBuilder("")
    var errorLines = new StringBuilder("")
    val logger = ProcessLogger(line => { println(line); normalLines ++= line+"\n" }, line => errorLines ++= line+"\n")
    val proc = Process(cmd,None,"CONFIG" -> d)
    proc ! logger
    if(errorLines.toString != "") println(errorLines.toString)
    if(normalLines.toString != "") println(normalLines.toString)
    (normalLines.toString,errorLines.toString)
  }
  def createDirectory(dir:String) = {
    val (n,e) = execute("mkdir -p " + dir)
  }
  def write(path:List[String],name:String,out:String) = {
    for( i <- 1 to path.size){
      createDirectory(path.slice(0,i).mkString("/"))
    }
    val all = path.mkString("/") + "/" + name
    val nwriter = new PrintWriter(new File(all))
    nwriter.write(out)
    nwriter.close()
  }
}

object Hammer {
  val usage = """
  Usage : sbt run-main 'Hammer -mode p <settingsClassName>
  Usage : sbt run-main 'Hammer -mode u <settingsClassName>
  Usage : sbt run-main 'Hammer -mode c -d <configClassName> <settingsClassName>
      - [-mode] :  [p]arent mode, [c]hild mode, [u]pdate mode.
      - [-d]    :  Single design
      - <settingsClassName> HammerSettings subclass name must be given
  """
  def main(args: Array[String]): Unit = {
    if (args.length == 0) println(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]
    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "-mode" :: value :: tail =>
                               nextOption(map ++ Map('mode -> value.toString), tail)
        case "-d" :: value :: tail =>
                               nextOption(map ++ Map('d -> value.toString), tail)
        case string :: opt2 :: tail if isSwitch(opt2) => 
                               nextOption(map ++ Map('set -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('set -> string), list.tail)
        case option :: tail => println("Unknown option "+option) 
                               exit(1) 
      }
    }
    val op = nextOption(Map(),arglist)
    val mode = op.get('mode)
    val set = op.get('set)
    val d = op.get('d)
    if(! o2b(mode)) throw new MissingOption("-mode")
    if(mode.get == "c" && !o2b(d)) throw new MissingOption("-d")
    if(! o2b(set)) throw new MissingOption("Setting Class Name")
    
    val settings = Class.forName(set.get.toString).newInstance.asInstanceOf[HammerSettings]

    if(mode.get == "c") {
      val design = d.get.toString
      settings.qors.foreach(q => {
          val ex = execute("bash "+settings.executeScripts(q),design)
          write(List(settings.outputsShared,q),design+settings.version+".OU",ex._1)
          write(List(settings.outputsShared,q),design+settings.version+".ER",ex._2)
          val pa = execute(settings.parseScripts(q),design)
          write(List(settings.resultsShared,q),design+settings.version+".OU",pa._1)
          write(List(settings.resultsShared,q),design+settings.version+".ER",pa._2)
        })
      if(settings.no_nscratch){
        execute(settings.copyScript,design+settings.version)
      }
    }
    if(mode.get == "p") {
      execute(Seq(settings.setupPath, settings.baseShared, settings.root).mkString(" "))
      settings.designs.foreach( d => {
          execute(Seq("./" + settings.qsubPath,d,settings.baseShared,settings.root,settings.baseLocal,d+settings.version,settings.RISCV,settings.launchPath).mkString(" "))
        })
    }
    if (mode.get == "u") {
    }
  }
}
