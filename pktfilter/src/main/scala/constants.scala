package pktfilter

object Constants {
  val IPv4LengthOffset = 2
  val IPv6LengthOffset = 4
  val IPv4ProtocolOffset = 9
  val IPv6ProtocolOffset = 6
  val IPv4SrcAddrOffset = 12
  val IPv4DstAddrOffset = 16
  val IPv4OptionsOffset = 20

  val MCMagic = 0x80
  val GetOpcode = 0x00

  val UdpProtocol = 0x11
  val TcpProtocol = 0x6
}
