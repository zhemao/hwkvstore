package pktfilter

import pktfilter.Constants._
import pktfilter.ChecksumUtils._

object IPv4Packet {
  def apply(prot: Int, srcaddr: Array[Byte], dstaddr: Array[Byte],
      data: Array[Byte], ttl: Int = 100): Array[Byte] = {
    val header = Array.fill(20) { 0.toByte }
    val pktSize = data.size + 20
    header(0) = (4 << 4 | 5).toByte
    srcaddr.copyToArray(header, IPv4SrcAddrOffset, 4)
    dstaddr.copyToArray(header, IPv4DstAddrOffset, 4)
    header(IPv4LengthOffset) = ((pktSize >> 8) & 0xff).toByte
    header(IPv4LengthOffset + 1) = (pktSize & 0xff).toByte
    header(IPv4ProtocolOffset) = prot.toByte
    header(8) = ttl.byteValue

    val checksum = computeChecksum(header)
    header(10) = ((checksum >> 8) & 0xff).byteValue
    header(11) = (checksum & 0xff).byteValue

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
      udpData: Array[Byte], ipv6: Boolean = false,
      ttl: Int = 100): Array[Byte] = {
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
    else {
      val packet = IPv4Packet(UdpProtocol, srcaddr, dstaddr, ipData, ttl)
      val pseudoPacket = packet.slice(12, 20) ++
        Array[Byte](0, UdpProtocol.byteValue) ++
        packet.slice(24, 26) ++ packet.slice(20, packet.length)
      val udpChecksum = computeChecksum(pseudoPacket)
      packet(26) = ((udpChecksum >> 8) & 0xff).byteValue
      packet(27) = (udpChecksum & 0xff).byteValue
      packet
    }
  }
}

object MemcachedGet {
  def apply(srcaddr: Array[Byte], srcport: Int,
      dstaddr: Array[Byte], dstport: Int,
      key: String, reqid: Int,
      ipv6: Boolean = false, ttl: Int = 100): Array[Byte] = {
    val header = Array.fill(32) { 0.toByte }
    header(0) = ((reqid >> 8) & 0xff).toByte
    header(1) = (reqid & 0xff).toByte
    header(5) = 1
    header(8) = MCMagic.toByte
    header(9) = GetOpcode.toByte
    header(11) = key.length.toByte
    header(19) = key.length.toByte

    val data = header ++ key.getBytes

    UdpPacket(srcaddr, srcport, dstaddr, dstport, data, ipv6, ttl)
  }
}

object MemcachedResp {
  def apply(srcaddr: Array[Byte], srcport: Int,
      dstaddr: Array[Byte], dstport: Int, value: String, reqid: Int,
      ipv6: Boolean = false, ttl: Int = 100): Array[Byte] = {
    val header = Array.fill(36) { 0.toByte }
    header(0) = ((reqid >> 8) & 0xff).toByte
    header(1) = (reqid & 0xff).toByte
    header(5) = 1
    // magic
    header(8) = 0x81.byteValue
    // extras length
    header(12) = 4
    // body length
    header(18) = ((value.length >> 8) & 0xff).byteValue
    header(19) = (value.length & 0xff).byteValue
    // Extras 0xDEADBEEF
    header(32) = 0xde.byteValue
    header(33) = 0xad.byteValue
    header(34) = 0xbe.byteValue
    header(35) = 0xef.byteValue

    val data = header ++ value.getBytes
    UdpPacket(srcaddr, srcport, dstaddr, dstport, data, ipv6, ttl)
  }
}
