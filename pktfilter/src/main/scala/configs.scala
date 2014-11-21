package pktfilter

import Chisel._

class PacketFilterConfig extends ChiselConfig {
  override val topDefinitions: World.TopDefs = {
    (pname,site,here) => pname match {
      case "keylensize"  => 8
      case "vallensize" => Knob("vallensize")
      case "tagsize" => 4
      case "bufsize" => 65536
    }
  }

  override val knobValues: Any=>Any = {
    case "vallensize" => 19
  }
}
