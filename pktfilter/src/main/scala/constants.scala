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

  val DefaultDstMac = Array[Byte](20, 10, 32, 11, 22, 13)
  val DefaultSrcMac = Array[Byte](38, 13, 53, 28, 17, 52)

  val IPv4EtherType = 0x0800
  val IPv6EtherType = 0x86DD
}
