/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import Chisel.Implicits._
import jackhammer._


class JackTestSuite extends AssertionsForJUnit {
  
/** This testsuite checks all basic constraint operations are typed correctly
*/
  //Boolean Expressions
  @Test def testAnd() {
    val a = ExLit[Boolean](true); val b = ExVar[Boolean]("b");
    val e = a && b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Boolean" )
    assertTrue( types(b) == "Boolean" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testOr() {
    val a = ExLit[Boolean](true); val b = ExVar[Boolean]("b");
    val e = a || b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Boolean" )
    assertTrue( types(b) == "Boolean" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testXor() {
    val a = ExLit[Boolean](false); val b = ExVar[Boolean]("b");
    val e = a ^ b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Boolean" )
    assertTrue( types(b) == "Boolean" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testEqBoolean() {
    val a = ExLit[Boolean](true); val b = ExVar[Boolean]("b");
    val e = a === b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Boolean" )
    assertTrue( types(b) == "Boolean" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testNeqBoolean() {
    val a = ExLit[Boolean](false); val b = ExVar[Boolean]("b");
    val e = a !== b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Boolean" )
    assertTrue( types(b) == "Boolean" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testEqInt() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a === b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testNeq() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a !== b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testLt() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a < b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testLte() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a <= b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testGt() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a > b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testGte() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a >= b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Boolean" )
  }

  // Integer Expressions
  @Test def testAdd() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a + b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Int" )
  }
  @Test def testSub() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a - b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Int" )
  }
  @Test def testMul() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a * b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Int" )
  }
  @Test def testMod() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a % b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Int" )
  }

  //Unknown Expressions
  @Test def testVarUnk() {
    val a = ExVar[Int]("a"); val b = ExVar[Int]("b");
    val e = a === b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Boolean" )
    assertTrue( types(b) == "Boolean" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testVarInt() {
    val a = ExVar[Int]("a"); val b = ExVar[Int]("b");
    val e = a > b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Int" )
    assertTrue( types(b) == "Int" )
    assertTrue( types(e) == "Boolean" )
  }
  @Test def testVarBool() {
    val a = ExVar[Boolean]("a"); val b = ExVar[Boolean]("b");
    val e = a && b
    val l = List(a,b,e)
    val types = Jack.assignTypes(l)
    assertTrue( types(a) == "Boolean" )
    assertTrue( types(b) == "Boolean" )
    assertTrue( types(e) == "Boolean" )
  }

  /** This testsuite checks all basic constraint operations are evaluated correctly
  */
  //Boolean Expressions
  @Test def testAndEval() {
    val a = ExLit[Boolean](true); val b = ExVar[Boolean]("b");
    val e = a && b
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "true" )
  }
  @Test def testOrEval() {
    val a = ExLit[Boolean](false); val b = ExVar[Boolean]("b");
    val e = a || b
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "true" )
  }
  @Test def testXorEval() {
    val a = ExLit[Boolean](false); val b = ExVar[Boolean]("b");
    val e = a ^ b
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "true" )
  }
  @Test def testEqBooleanEval() {
    val a = ExLit[Boolean](true); val b = ExVar[Boolean]("b");
    val e = a === b
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "true" )
  }
  @Test def testNeqBooleanEval() {
    val a = ExLit[Boolean](false); val b = ExVar[Boolean]("b");
    val e = a !== b
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "true" )
  }

  //Boolean Expressions on Ints
  @Test def testLtEval() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b"); val c = ExLit[Int](12)
    val e1 = a < b
    val e2 = b < c
    val l = List(e1,e2)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "11" )
  }
  @Test def testLteEval() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b"); val c = ExLit[Int](10)
    val e1 = a <= b
    val e2 = b <= c
    val l = List(e1,e2)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "10" )
  }
  @Test def testGtEval() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b"); val c = ExLit[Int](12)
    val e1 = c > b
    val e2 = b > a
    val l = List(e1,e2)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "11" )
  }
  @Test def testGteEval() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b"); val c = ExLit[Int](10)
    val e1 = a >= b
    val e2 = b >= c
    val l = List(e1,e2)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "10" )
  }

  // Integer Expressions
  @Test def testEqIntEval() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val e = a === b
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "10" )
  }
  @Test def testNeqIntEval() {
    val a = ExLit[Int](10); val b = ExVar[Int]("b");
    val c = ExLit[Int](9); val d = ExLit[Int](12);
    val e1 = (a !== b) && (c < b) && (b < d)
    val l = List(e1)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$b") == "11" )
  }
  // Integer Expressions
  @Test def testAddEval() {
    val a = ExLit[Int](10); val b = ExLit[Int](5); val c = ExVar[Int]("c")
    val e = (c === (a + b))
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$c") == "15" )
  }
  @Test def testSubEval() {
    val a = ExLit[Int](10); val b = ExLit[Int](5); val c = ExVar[Int]("c")
    val e = (c === (a - b))
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$c") == "5" )
  }
  @Test def testMulEval() {
    val a = ExLit[Int](10); val b = ExLit[Int](5); val c = ExVar[Int]("c")
    val e = (c === (a * b))
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$c") == "50" )
  }
  @Test def testModEval() {
    val a = ExLit[Int](10); val b = ExLit[Int](5); val c = ExVar[Int]("c")
    val e = (c === (a % b))
    val l = List(e)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$c") == "0" )
  }

  //Unknown Expressions
  @Test def testVarUnkEval() {
    val a = ExVar[Int]("a"); val b = ExVar[Int]("b");
    val c = ExLit[Int](10); val d = ExLit[Int](10);
    val e1 = a <= c
    val e2 = b >= d
    val e3 = a === b
    val l = List(e1,e2,e3)
    val maps = Jack.getKnobMaps(l,Jack.assignTypes(l.map(x=>Ex.unfurl(x)).reduce(_ ::: _)))
    assertTrue( maps(0)("$a") == "10" )
    assertTrue( maps(0)("$b") == "10" )
  }
}

