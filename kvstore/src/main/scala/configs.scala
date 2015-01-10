package kvstore

import Chisel._

class BaseConfig extends ChiselConfig {
  private def isPowerOfTwo(num: Ex[Int], start: Int, end: Int) = {
    var expr: Ex[Boolean] = ExEq(num, ExLit(start))
    var check = 2 * start
    while (check <= end) {
      expr = ExOr(expr, ExEq(num, ExLit(check)))
      check = 2 * check
    }
    expr
  }
  override val topDefinitions:World.TopDefs = {
    (pname,site,here) => pname match {
      case "keywordsize" => Knob("keywordsize")
      case "keysize"  => 256
      case "numkeys"  => Knob("numkeys")
      case "valcachesize" => Knob("valcachesize")
      case "valwordsize" => Knob("valwordsize")
      case "banksize" => Knob("banksize")
      case "tagsize" => 6
      case "countsize" => 4
      case "maxfanin" => Knob("maxfanin")
      case "bankmems" => Knob("bankmems")
      case "physaddr" => Knob("physaddr")
    }
  }
  override val topConstraints:List[ViewSym=>Ex[Boolean]] = List(
    ex => isPowerOfTwo(ex[Int]("keywordsize"), 8, 64),
    ex => isPowerOfTwo(ex[Int]("valwordsize"), 8, 16),
    ex => isPowerOfTwo(ex[Int]("numkeys"), 32, 1024),
    ex => isPowerOfTwo(ex[Int]("valcachesize"), 1024, 1024 * 1024),
    ex => isPowerOfTwo(ex[Int]("maxfanin"), 4, 32),
    ex => isPowerOfTwo(ex[Int]("banksize"), 256, 1024)
  )
}

class VlsiConfig extends BaseConfig {
  override val knobValues:Any=>Any = {
    case "keywordsize" => 32
    case "numkeys" => 64
    case "valcachesize" => 32 * 1024
    case "valwordsize" => 16
    case "maxfanin" => 16
    case "bankmems" => true
    case "banksize" => 256
    case "physaddr" => false
  }
}

class EmulatorConfig extends BaseConfig {
  override val knobValues:Any=>Any = {
    case "keywordsize" => 32
    case "numkeys" => 256
    case "valcachesize" => 128 * 1024
    case "valwordsize" => 16
    case "maxfanin" => 32
    case "bankmems" => true
    case "banksize" => 1024
    case "physaddr" => true
  }
}

class FpgaConfig extends BaseConfig {
  override val knobValues:Any=>Any = {
    case "keywordsize" => 32
    case "numkeys" => 1024
    case "valcachesize" => 512 * 1024
    case "valwordsize" => 16
    case "maxfanin" => 32
    case "bankmems" => false
    case "banksize" => 1024
    case "physaddr" => false
  }
}
