package McAccel

import Chisel._
import McAccel.TestUtils._

class BankedMem(val WordSize: Int, val BankSize: Int, val NumBanks: Int)
    extends Module {
  val TotalSize = BankSize * NumBanks

  val FullAddrSize = log2Up(TotalSize)
  val BankAddrSize = log2Up(BankSize)

  val io = new Bundle {
    val readAddr = UInt(INPUT, FullAddrSize)
    val readData = UInt(OUTPUT, WordSize)

    val writeAddr = UInt(INPUT, FullAddrSize)
    val writeData = UInt(INPUT, WordSize)
    val writeEn   = Bool(INPUT)
  }

  val readAddrReg  = Reg(next = io.readAddr)
  val writeAddrReg = Reg(next = io.writeAddr)
  val writeDataReg = Reg(next = io.writeData)
  val writeEnReg   = Reg(next = io.writeEn)

  val bankReadAddr0 = readAddrReg(BankAddrSize - 1, 0)
  val bankReadSel0  = readAddrReg(FullAddrSize - 1, BankAddrSize)
  val bankReadSel1  = Reg(next = bankReadSel0)

  val bankWriteAddr = writeAddrReg(BankAddrSize - 1, 0)
  val bankWriteSel  = writeAddrReg(FullAddrSize - 1, BankAddrSize)
  val bankWriteEn   = Vec((0 until NumBanks).map {
    i => writeEnReg && bankWriteSel === UInt(i)
  })

  val banks = Array.fill(NumBanks) {
    Mem(UInt(width = WordSize), BankSize, true)
  }
  val bankReadData1 = Vec.fill(NumBanks) { Reg(UInt(width = WordSize)) }

  for (i <- 0 until NumBanks) {
    val bank = banks(i)
    bankReadData1(i) := bank(bankReadAddr0)
    when (bankWriteEn(i)) {
      bank(bankWriteAddr) := writeDataReg
    }
  }

  val readData1 = bankReadData1(bankReadSel1)
  val readData2 = Reg(next = readData1)
  io.readData := readData2
}

class BankedMemTester(c: BankedMem) extends Tester(c) {
  val NumTests = 10
  val ReadDelay = 3
  val addrs =  Array.fill(NumTests) { rnd.nextInt(c.TotalSize) }
  val values = Array.fill(NumTests) { rnd.nextInt(1 << c.WordSize) }

  poke(c.io.writeEn, 1)
  for (i <- 0 until NumTests) {
    poke(c.io.writeAddr, addrs(i))
    poke(c.io.writeData, values(i))
    step(1)
  }
  poke(c.io.writeEn, 0)

  for (i <- 0 until ReadDelay) {
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  for (i <- ReadDelay until NumTests) {
    expect(c.io.readData, values(i - ReadDelay))
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  for (i <- (NumTests - ReadDelay) until NumTests) {
    expect(c.io.readData, values(i))
    step(1)
  }
}

object BankedMemMain {
  def main(args: Array[String]) {
    chiselMain(args, () => Module(new BankedMem(16, 128, 16)),
      (c: BankedMem) => new BankedMemTester(c))
  }
}
