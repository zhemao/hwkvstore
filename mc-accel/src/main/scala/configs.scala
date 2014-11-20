package McAccel

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
      case "wordsize" => Knob("wordsize")
      case "keysize"  => 256
      case "numkeys"  => Knob("numkeys")
      case "valcachesize" => Knob("valcachesize")
      case "tagsize" => 16
      case "maxfanin" => Knob("maxfanin")
      case "bankmems" => Knob("bankmems")
    }
  }
  override val topConstraints:List[ViewSym=>Ex[Boolean]] = List(
    ex => isPowerOfTwo(ex[Int]("wordsize"), 8, 64),
    ex => isPowerOfTwo(ex[Int]("numkeys"), 128, 1024),
    ex => isPowerOfTwo(ex[Int]("valcachesize"), 1024, 1024 * 1024),
    ex => isPowerOfTwo(ex[Int]("maxfanin"), 4, 32)
  )
}

class VlsiConfig extends BaseConfig {
  override val knobValues:Any=>Any = {
    case "wordsize" => 32
    case "numkeys" => 128
    case "valcachesize" => 64 * 1024
    case "maxfanin" => 16
    case "bankmems" => true
  }
}

class EmulatorConfig extends BaseConfig {
  override val knobValues:Any=>Any = {
    case "wordsize" => 32
    case "numkeys" => 256
    case "valcachesize" => 128 * 1024
    case "maxfanin" => 32
    case "bankmems" => false
  }
}
