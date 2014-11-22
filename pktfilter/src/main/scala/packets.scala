package pktfilter

import pktfilter.Constants._

object IPv4Packet {
  def apply(prot: Byte, data: Array[Byte]): Array[Byte] = {
    val header = Array.fill(20) { 0.toByte }
    val pktSize = data.size + 20
    header(0) = (4 << 4 | 5).toByte
    header(IPv4LengthOffset) = ((pktSize >> 8) & 0xff).toByte
    header(IPv4LengthOffset + 1) = (pktSize & 0xff).toByte
    header(IPv4ProtocolOffset) = prot

    header ++ data
  }
}

object IPv6Packet {
  def apply(prot: Byte, data: Array[Byte]): Array[Byte] = {
    val header = Array.fill(40) { 0.toByte }
    val pktSize = data.size + 40
    header(0) = (6 << 4).toByte
    header(IPv6LengthOffset) = ((pktSize >> 8) & 0xff).toByte
    header(IPv6LengthOffset + 1) = (pktSize & 0xff).toByte
    header(IPv6ProtocolOffset) = prot

    header ++ data
  }
}
