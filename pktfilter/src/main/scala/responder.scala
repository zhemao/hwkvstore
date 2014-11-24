package pktfilter

import Chisel._
import pktfilter.Constants._

class Responder(AddrSize: Int) extends Module {
  val io = new Bundle {
    val temac_tx = Stream(UInt(width = 8))
    val resultData = Decoupled(UInt(width = 8)).flip
    val resLen = UInt(INPUT, AddrSize)
    val pktRoute = new RoutingInfo().asInput
    val start = Bool(INPUT)
    val ready = Bool(OUTPUT)
  }

  // IP header + UDP header + Memcached header
  val HeaderLen = 20 + 8 + 28

  val headerIndex = Reg(UInt(width = log2Up(HeaderLen)))
  val headerData = UInt(width = 8)

  val ipPktLen = io.resLen + UInt(HeaderLen)
  val udpPktLen = io.resLen + UInt(8 + 28)
  val mcPktLen = io.resLen + UInt(28)

  val ipChecksum = Reg(UInt(width = 16))
  val udpChecksum = Reg(UInt(width = 16))

  val TTL = 100

  switch(headerIndex) {
    // default value
    headerData := UInt(0)

    // IP header
    // version is 4 and header len is 5
    is (UInt(0))  { headerData := UInt(0x45) }
    // total packet length
    is (UInt(2))  { headerData := ipPktLen(15, 8) }
    is (UInt(3))  { headerData := ipPktLen(7, 0) }
    // TTL
    is (UInt(8))  { headerData := UInt(TTL) }
    // Protocol
    is (UInt(9))  { headerData := UInt(UdpProtocol) }
    // IP Header Checksum
    is (UInt(10)) { headerData := ipChecksum(15, 8) }
    is (UInt(11)) { headerData := ipChecksum(7, 0) }
    // source address (src and dst switched for reply)
    is (UInt(12)) { headerData := io.pktRoute.dstAddr(31, 24) }
    is (UInt(13)) { headerData := io.pktRoute.dstAddr(23, 16) }
    is (UInt(14)) { headerData := io.pktRoute.dstAddr(15, 8) }
    is (UInt(15)) { headerData := io.pktRoute.dstAddr(7, 0) }
    // dest address
    is (UInt(16)) { headerData := io.pktRoute.srcAddr(31, 24) }
    is (UInt(17)) { headerData := io.pktRoute.srcAddr(23, 16) }
    is (UInt(18)) { headerData := io.pktRoute.srcAddr(15, 8) }
    is (UInt(19)) { headerData := io.pktRoute.srcAddr(7, 0) }

    // UDP Header
    // source port
    is (UInt(20)) { headerData := io.pktRoute.dstPort(15, 8) }
    is (UInt(21)) { headerData := io.pktRoute.dstPort(7, 0) }
    // dest port
    is (UInt(22)) { headerData := io.pktRoute.srcPort(15, 8) }
    is (UInt(23)) { headerData := io.pktRoute.srcPort(7, 0) }
    // udp packet length
    is (UInt(24)) { headerData := udpPktLen(15, 8) }
    is (UInt(25)) { headerData := udpPktLen(7, 0) }
    // UDP checksum
    is (UInt(26)) { headerData := udpChecksum(15, 8) }
    is (UInt(27)) { headerData := udpChecksum(7, 0) }

    // Memcached Header
    // Magic
    is (UInt(28)) { headerData := UInt(0x81) }
    // Extra length
    is (UInt(32)) { headerData := UInt(4) }
    // body length
    is (UInt(38)) { headerData := io.resLen(15, 8) }
    is (UInt(39)) { headerData := io.resLen(7, 0) }
    // Extras 0xDEADBEEF
    is (UInt(52)) { headerData := UInt(0xde) }
    is (UInt(53)) { headerData := UInt(0xad) }
    is (UInt(54)) { headerData := UInt(0xbe) }
    is (UInt(55)) { headerData := UInt(0xef) }
  }
}
