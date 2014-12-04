/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package jackhammer
import scala.collection.mutable.{HashMap,Map,LinkedList}
import scala.util.Random
import java.lang.reflect.{Type, ParameterizedType}
import scala.io.Source
import java.io._

import jp.kobe_u.copris._
import jp.kobe_u.copris.dsl._
import Chisel.{Bool => CBool, _} //Renaming Chisel.Bool because of name conflict

class UnknownOption(option:String, cause:Throwable=null) extends RuntimeException("Unknown option: " + option, cause)
class MissingOption(option:String, cause:Throwable=null) extends RuntimeException("Missing option: " + option, cause)
class EmptyCstFile(file:String, cause:Throwable=null) extends RuntimeException("Empty cst file: " + file, cause)

class UnknownParameterType(field:Any, cause:Throwable=null) extends RuntimeException("Parameter " + field + " has an unknown type.", cause)
class NoSolutionException(cause:Throwable=null) extends RuntimeException("No solution found.", cause)

object Jack {
  val knobMap = HashMap[String,Expr]()
  val knobType = HashMap[String,String]()

  val usage = """
    Usage: sbt run-main 'Jack -c class -p project -d config_dir [-r #randompoints] [output_name]'
      - Default is an exhaustive dump of all possible parameterizations (unless -r is specified)
      - Default output name is config_dir/project.class.scala (unless output_name is given)
  """
  def main(args: Array[String]) {
    if (args.length == 0) println(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]
    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def fixDir(d:String) = if(d.endsWith("/")) d else d + "/"
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "-r" :: value :: tail =>
                               nextOption(map ++ Map('r -> value.toInt), tail)
        case "-d" :: value :: tail =>
                               nextOption(map ++ Map('d -> fixDir(value.toString)), tail)
        case "-c" :: value :: tail =>
                               nextOption(map ++ Map('c -> value.toString), tail)
        case "-p" :: value :: tail =>
                               nextOption(map ++ Map('p -> value.toString), tail)
        case string :: opt2 :: tail if isSwitch(opt2) => 
                               nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
        case option :: tail => println("Unknown option "+option) 
                               exit(1) 
      }
    }
    val options = nextOption(Map(),arglist)
    val dir  = if(options.contains('d)) options('d).toString else { throw new MissingOption("-d"); "" }
    val cls  = if(options.contains('c)) options('c).toString else { throw new MissingOption("-c"); "" }
    val proj = if(options.contains('p)) options('p).toString else { throw new MissingOption("-p"); "" }
    val cst  = dir +proj+ "."+ cls + ".cst"
    val num  = if(options.contains('r)) Some(options('r).asInstanceOf[Int]) else None
    val out  = if(options.contains('infile)) options('infile).toString else dir + proj +"."+ cls + ".scala";

    //Execute
    val pile = parseCst(cst)
    val tys = if(!pile.isEmpty) assignTypes( pile.map(x => Ex.unfurl(x)).reduce(_ ::: _) ) else { throw new EmptyCstFile(cst); Map[Ex[_],String]() }
    val knobMaps = getKnobMaps(pile,tys)
    //prettyPrint(knobMaps)
    println("Writing configuration classes to " + out)
    val knobMapSubset = if(num.isDefined) sample(knobMaps,num.get) else knobMaps
    writeConfigs(out,cls,proj,knobMapSubset)
  }
  def sample(knobMaps:List[Map[String,String]],num:Int):List[Map[String,String]] = {
    if(knobMaps.size < num) knobMaps else {
      val i = Random.nextInt(knobMaps.size)
      if(num != 1) {
        List(knobMaps(i)) ::: sample(knobMaps.diff(List(i)),num-1)
      } else List(knobMaps(i))
    }
  }
  def prettyPrint(kmps:List[Map[String,String]]) = kmps.map(kmp => println(kmp.map(s => (s._1 + " -> " + s._2)).reduce(_ + ", " + _)))
  def parseCst(filename:String):List[Ex[_]] = {
    var pile = List[Ex[_]]()
    for (line <- Source.fromFile(filename).getLines()) {
      pile = furl[Boolean](line.split(" ").toList) :: pile
    }
    pile
  }
  def peel(tokens:List[String]):List[String] = tokens.slice(1,tokens.size-1)
  def furl[T](tokens:List[String]):Ex[T] = {
    val tk = peel(tokens)
    if(tk.size == 1) tk(0) match {
      case t if t.startsWith("$") => ExVar[T](tk(0).tail)
      case "true" => ExLit[T](true.asInstanceOf[T])
      case "false" => ExLit[T](false.asInstanceOf[T])
      case s:String => ExLit[T](s.toInt.asInstanceOf[T])
    } else {
      getOp(tk, 0) match {
        case "+"  => ExAdd(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case "-"  => ExSub(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case "*"  => ExMul(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case "%"  => ExMod(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case "<=" => ExLte(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case "<"  =>  ExLt(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case ">=" => ExGte(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case ">"  =>  ExGt(leftEx[Int](tk),rightEx[Int](tk)).asInstanceOf[Ex[T]]
        case "="  =>  ExEq(leftEx[Any](tk),rightEx[Any](tk)).asInstanceOf[Ex[T]]
        case "!=" => ExNeq(leftEx[Any](tk),rightEx[Any](tk)).asInstanceOf[Ex[T]]
        case "&&" => ExAnd(leftEx[Boolean](tk),rightEx[Boolean](tk)).asInstanceOf[Ex[T]]
        case "||" =>  ExOr(leftEx[Boolean](tk),rightEx[Boolean](tk)).asInstanceOf[Ex[T]]
        case "^"  => ExXor(leftEx[Boolean](tk),rightEx[Boolean](tk)).asInstanceOf[Ex[T]]
      }
    }
  }
  def leftEx[T](tokens:List[String]):Ex[T]  = getEx[T](tokens,0,0,0,0)
  def rightEx[T](tokens:List[String]):Ex[T] = getEx[T](tokens,0,0,0,1)
  def getEx[T](tokens:List[String],counter:Int,pointer:Int,num:Int,sel:Int):Ex[T] = { //ParserCombinator
    if(pointer < tokens.size && tokens(pointer) == "(") getEx[T](tokens,counter+1,pointer+1,num,sel) else
    if(pointer < tokens.size && tokens(pointer) == ")") getEx[T](tokens,counter-1,pointer+1,num,sel) else
    if(counter == 0 ){
      if(num != sel) {
        getEx[T](tokens.slice(pointer+1,tokens.size),counter,0,num+1,sel) 
      } else furl[T](tokens.slice(0,pointer))
    } else getEx[T](tokens,counter,pointer+1,num,sel)
  }
  def getOp(tokens:List[String],counter:Int):String = {
    if(tokens.head == "(") getOp(tokens.tail,counter+1) else 
    if(tokens.head == ")") getOp(tokens.tail,counter-1) else 
    if(counter == 0) tokens.head else getOp(tokens.tail,counter)
  }
  

  def assignTypes(pile:List[Ex[_]]):Map[Ex[_],String] = {
    init
    var exTypeVar = Map[Ex[_],Bool]()
    var exTypeVal = Map[Ex[_],String]()
    var count = 0
    def assign(m:Map[Ex[_],String],e:Ex[_],b:Boolean) = {
      val t = if(b) "Boolean" else "Int"
      if(!m.contains(e) || (m(e) == "Int" && t == "Boolean")) m += ((e,t))
    }
    def ty(e:Ex[_]):Bool = {
      exTypeVar.getOrElse(e,{
        val b = bool(Bool("t"+count.toString));
        exTypeVar += ((e,b))
        count = count + 1
        b
      })
    }

    pile.map(t => t match {
        case ExAdd(a,b) => { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> FALSE)} //FALSE = Int
        case ExSub(a,b) => { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> FALSE)} //TRUE = Boolean
        case ExMul(a,b) => { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> FALSE)}
        case ExMod(a,b) => { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> FALSE)}
        case ExGt(a,b) =>  { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> TRUE)}
        case ExGte(a,b) => { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> TRUE)}
        case ExLt(a,b) =>  { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> TRUE)}
        case ExLte(a,b) => { add(ty(a) <==> FALSE) ; add(ty(b) <==> FALSE) ; add(ty(t) <==> TRUE)}
        case ExAnd(a,b) => { add(ty(a) <==> TRUE)  ; add(ty(b) <==> TRUE)  ; add(ty(t) <==> TRUE)}
        case ExOr(a,b) =>  { add(ty(a) <==> TRUE)  ; add(ty(b) <==> TRUE)  ; add(ty(t) <==> TRUE)}
        case ExXor(a,b) => { add(ty(a) <==> TRUE)  ; add(ty(b) <==> TRUE)  ; add(ty(t) <==> TRUE)}
        case ExEq(a,b) =>  { add(ty(a) <==> ty(b)) ; add(ty(t) <==> TRUE) }
        case ExNeq(a,b) => { add(ty(a) <==> ty(b)) ; add(ty(t) <==> TRUE) }
        case ExLit(lit:Int) => add(ty(ExLit[Int](lit)) <==> FALSE )
        case ExLit(lit:Boolean) => add(ty(ExLit[Boolean](lit)) <==> TRUE )
        case v:ExVar[_] => {ty(v)}
      })
    if(find){
      do {
        pile.map(e => assign(exTypeVal,e,solutions.next.boolValues(exTypeVar(e))))
      } while (findNext)
    }
    exTypeVal
  }

  def getKnobMaps(pile:List[Ex[_]],types:Map[Ex[_],String]):List[Map[String,String]] = {
    val min = -100000; val max = 100000
    var exBoolVar = Map[Ex[_],Bool]()
    var exIntVar = Map[Ex[_],Term]()
    def vrb[T](e:Ex[_]):T = types(e) match {
      case "Int"     => {
        if(!exIntVar.contains(e)) {
          val i = int(Var(e.toString),min,max); exIntVar += ((e,i)); i.asInstanceOf[T]
        } else exIntVar(e).asInstanceOf[T]
      }
      case "Boolean" => {
        if(!exBoolVar.contains(e)) { 
          val b = bool(Bool(e.toString)); exBoolVar += ((e,b)); b.asInstanceOf[T]
        } else exBoolVar(e).asInstanceOf[T]
      }
      case _ => throw new UnknownParameterType(e)
    }
    def convert[T](e:Ex[_]):T = ( types(e) match {
      case "Int"     => {
        e match {
          case ExVar(name:String) => vrb[Term](e)
          case ExLit(lit:Int) => Num(lit)
          case ExAdd(a,b) => convert[Term](a) + convert[Term](b)
          case ExSub(a,b) => convert[Term](a) - convert[Term](b)
          case ExMul(a,b) => convert[Term](a) * convert[Term](b)
          case ExMod(a,b) => convert[Term](a) % convert[Term](b)
        }
      }
      case "Boolean" => {
        e match {
          case ExVar(name:String) => vrb[Constraint](e)
          case ExLit(lit:Boolean) => if(lit) TRUE else FALSE
          case ExGt(a,b)  => convert[Term](a) >  convert[Term](b)
          case ExGte(a,b) => convert[Term](a) >= convert[Term](b)
          case ExLt(a,b)  => convert[Term](a) <  convert[Term](b)
          case ExLte(a,b) => convert[Term](a) <= convert[Term](b)
          case ExAnd(a,b) => convert[Constraint](a) &&   convert[Constraint](b)
          case ExOr(a,b)  => convert[Constraint](a) ||   convert[Constraint](b)
          case ExXor(a,b) => convert[Constraint](a) ^    convert[Constraint](b)
          case ExEq(a,b)  => {
            types(a) match {
              case "Int"     => convert[Term](a) === convert[Term](b)
              case "Boolean" => convert[Constraint](a) <==> convert[Constraint](b)
            }
          }
          case ExNeq(a,b) => {
            types(a) match {
              case "Int"     => convert[Term](a) =/= convert[Term](b)
              case "Boolean" => convert[Constraint](a) ^ convert[Constraint](b)
            }
          }
        }
      }
    }).asInstanceOf[T]

    //run
    init
    pile.map(e => add(convert[Constraint](e)))
    var knobMaps = List[Map[String,String]]()
    if(find){
      do {
        var knobMap = Map[String,String]()
        val i = solutions.next.intValues
        val b = solutions.next.boolValues
        i.map(s => s._1.name match {
            case k if k.startsWith("$") => knobMap += ((k,s._2.toString))
            case _ => {}
          })
        b.map(s => s._1.name match {
            case k if k.startsWith("$") => knobMap += ((k,s._2.toString))
            case _ => {}
          })
        knobMaps = knobMaps ++ List(knobMap)
        //println(knobMap)
      } while (findNext)
    }
    knobMaps
  }

  def writeConfigs(file:String,cls:String,proj:String,knobMaps:List[Map[String,String]]) = {
    if(knobMaps.isEmpty) throw new NoSolutionException()
    val out = "package " + proj + "\n\n" + knobMaps.zipWithIndex.map(v => {
      val s = v._1; val i = v._2;
      constructChiselConfig(cls,s,i)
    }).reduce(_ + "\n" + _)
    val f = new java.io.FileWriter(file)
    f.write(out)
    f.close
  }
  def constructChiselConfig(cls:String,knobMap:Map[String,String],i:Int):String = {
    val s = new StringBuilder("")
    s ++= "class " + cls + i.toString + " extends " + cls + " {\n"
    s ++= "  override val knobValues:Any=>Any = {\n"
    knobMap.map(k => {
      s ++= "    case \"" + k._1.tail + "\" => " + k._2 + "\n"
    })
    s ++= "  }\n}\n"
    s.toString
  }
}
