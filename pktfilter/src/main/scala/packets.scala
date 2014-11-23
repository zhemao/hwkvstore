package pktfilter

import pktfilter.Constants._

object IPv4Packet {
  def apply(prot: Int, srcaddr: Array[Byte], dstaddr: Array[Byte],
      data: Array[Byte]): Array[Byte] = {
    val header = Array.fill(20) { 0.toByte }
    val pktSize = data.size + 20
    header(0) = (4 << 4 | 5).toByte
    srcaddr.copyToArray(header, IPv4SrcAddrOffset, 4)
    dstaddr.copyToArray(header, IPv4DstAddrOffset, 4)
    header(IPv4LengthOffset) = ((pktSize >> 8) & 0xff).toByte
    header(IPv4LengthOffset + 1) = (pktSize & 0xff).toByte
    header(IPv4ProtocolOffset) = prot.toByte

    header ++ data
  }
}

object IPv6Packet {
  def apply(prot: Int, data: Array[Byte]): Array[Byte] = {
    val header = Array.fill(40) { 0.toByte }
    val pktSize = data.size + 40
    header(0) = (6 << 4).toByte
    header(IPv6LengthOffset) = ((pktSize >> 8) & 0xff).toByte
    header(IPv6LengthOffset + 1) = (pktSize & 0xff).toByte
    header(IPv6ProtocolOffset) = prot.toByte

    header ++ data
  }
}

object UdpPacket {
  def apply(srcaddr: Array[Byte], srcport: Int,
      dstaddr: Array[Byte], dstport: Int,
      udpData: Array[Byte], ipv6: Boolean = false): Array[Byte] = {
    val header = Array.fill(8) { 0.toByte }
    val pktSize = 8 + udpData.length
    header(0) = ((srcport >> 8) & 0xff).toByte
    header(1) = (srcport & 0xff).toByte
    header(2) = ((dstport >> 8) & 0xff).toByte
    header(3) = (dstport& 0xff).toByte
    header(4) = ((pktSize >> 8) & 0xff).toByte
    header(5) = (pktSize & 0xff).toByte

    val ipData = header ++ udpData
    if (ipv6)
      IPv6Packet(UdpProtocol, ipData)
    else
      IPv4Packet(UdpProtocol, srcaddr, dstaddr, ipData)
  }
}

object MemcachedGet {
  def apply(srcaddr: Array[Byte], srcport: Int,
      dstaddr: Array[Byte], dstport: Int,
      key: String, ipv6: Boolean = false): Array[Byte] = {
    val header = Array.fill(24) { 0.toByte }
    header(0) = MCMagic.toByte
    header(1) = GetOpcode.toByte
    header(3) = key.length.toByte
    header(11) = key.length.toByte

    val data = header ++ key.getBytes

    UdpPacket(srcaddr, srcport, dstaddr, dstport, data, ipv6)
  }
}
