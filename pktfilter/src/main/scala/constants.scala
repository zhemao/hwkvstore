package pktfilter

object Constants {
  val IPv4LengthOffset = 2
  val IPv6LengthOffset = 4
  val IPv4ProtocolOffset = 9
  val IPv6ProtocolOffset = 6

  val UdpProtocol = 0x11.toByte
  val TcpProtocol = 0x6.toByte
}
