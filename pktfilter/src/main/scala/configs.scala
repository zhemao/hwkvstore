package pktfilter

import Chisel._

class PacketFilterConfig extends ChiselConfig {
  override val topDefinitions: World.TopDefs = {
    (pname,site,here) => pname match {
      case "keylensize"  => 8
      case "vallensize" => 19
      case "valwordsize" => 16
      case "respcachesize" => 4096
      case "tagsize" => 4
      case "bufsize" => 65536
    }
  }
}

class CombinedConfig extends ChiselConfig(
  new PacketFilterConfig ++ new kvstore.EmulatorConfig)
