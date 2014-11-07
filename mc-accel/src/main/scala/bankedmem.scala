package McAccel

import Chisel._
import McAccel.TestUtils._
import McAccel.Constants.MemReadDelay

class UnbankedMem(val WordSize: Int, val MemSize: Int) extends Module {
  val AddrSize = log2Up(MemSize)
  val io = new Bundle {
    val readAddr = UInt(INPUT, AddrSize)
    val readData = UInt(OUTPUT, WordSize)

    val writeAddr = UInt(INPUT, AddrSize)
    val writeData = UInt(INPUT, WordSize)
    val writeEn   = Bool(INPUT)
  }

  val readAddrReg  = Reg(next = io.readAddr)
  val writeAddrReg = Reg(next = io.writeAddr)
  val writeDataReg = Reg(next = io.writeData)
  val writeEnReg   = Reg(next = io.writeEn)

  val mem = Mem(UInt(width = WordSize), MemSize)

  val readData = mem(readAddrReg)

  io.readData := ShiftRegister(readData, 3)

  when (writeEnReg) {
    mem(writeAddrReg) := writeDataReg
  }
}

class BankedMem(val WordSize: Int, val BankSize: Int, val NumBanks: Int)
    extends Module {
  val TotalSize = BankSize * NumBanks

  val FullAddrSize = log2Up(TotalSize)
  val BankAddrSize = log2Up(BankSize)
  val BankSelSize = FullAddrSize - BankAddrSize
  val PrimaryBankSelSize = BankSelSize / 2
  val SecondaryBankSelSize = BankSelSize - PrimaryBankSelSize

  val io = new Bundle {
    val readAddr = UInt(INPUT, FullAddrSize)
    val readData = UInt(OUTPUT, WordSize)

    val writeAddr = UInt(INPUT, FullAddrSize)
    val writeData = UInt(INPUT, WordSize)
    val writeEn   = Bool(INPUT)
  }

  val bankReadAddr0 = io.readAddr(BankAddrSize - 1, 0)
  val bankReadSel0  = io.readAddr(FullAddrSize - 1, BankAddrSize)
  val bankReadAddr1 = Reg(next = bankReadAddr0)
  val bankReadSel1  = Reg(next = bankReadSel0)
  val bankReadSel2  = Reg(next = bankReadSel1)

  val bankWriteAddr0 = io.writeAddr(BankAddrSize - 1, 0)
  val bankWriteSel0  = io.writeAddr(FullAddrSize - 1, BankAddrSize)
  val bankWriteAddr1 = Reg(next = bankWriteAddr0)
  val bankWriteSel1  = Reg(next = bankWriteSel0)

  val writeDataReg = Reg(next = io.writeData)
  val writeEnReg   = Reg(next = io.writeEn)

  val bankWriteEn = Vec((0 until NumBanks).map {
    i => writeEnReg && bankWriteSel1 === UInt(i)
  })

  val banks = Array.fill(NumBanks) {
    Mem(UInt(width = WordSize), BankSize, true)
  }
  val bankReadData2 = Vec.fill(NumBanks) { Reg(UInt(width = WordSize)) }

  for (i <- 0 until NumBanks) {
    val bank = banks(i)
    bankReadData2(i) := bank(bankReadAddr1)
    when (bankWriteEn(i)) {
      bank(bankWriteAddr1) := writeDataReg
    }
  }

  if (NumBanks > 2) {
    val NumSecondaryBanks = 1 << SecondaryBankSelSize
    val NumPrimaryBanks = 1 << PrimaryBankSelSize
    val bankReadData3 = Vec.fill(NumSecondaryBanks) {
      Reg(UInt(width = WordSize))
    }
    val primBankSel = bankReadSel2(PrimaryBankSelSize - 1, 0)
    for (i <- 0 until NumSecondaryBanks) {
      val subBankData = Vec.fill(NumPrimaryBanks) { UInt(width = WordSize) }
      for (j <- 0 until NumPrimaryBanks) {
        subBankData(j) := bankReadData2(i * NumPrimaryBanks + j)
      }
      bankReadData3(i) := subBankData(primBankSel)
    }
    val secBankSel = Reg(
      next = bankReadSel2(BankSelSize - 1, PrimaryBankSelSize))
    val readData3 = bankReadData3(secBankSel)
    val readData4 = Reg(next = readData3)
    io.readData := readData4
  } else {
    val readData2 = bankReadData2(bankReadSel2)
    io.readData := ShiftRegister(readData2, 2)
  }
}

class BankedMemTester(c: BankedMem) extends Tester(c) {
  val NumTests = 10
  val addrs =  Array.fill(NumTests) { rnd.nextInt(c.TotalSize) }
  val values = Array.fill(NumTests) { rnd.nextInt(1 << c.WordSize) }

  poke(c.io.writeEn, 1)
  for (i <- 0 until NumTests) {
    poke(c.io.writeAddr, addrs(i))
    poke(c.io.writeData, values(i))
    step(1)
  }
  poke(c.io.writeEn, 0)

  for (i <- 0 until MemReadDelay) {
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  for (i <- MemReadDelay until NumTests) {
    expect(c.io.readData, values(i - MemReadDelay))
    poke(c.io.readAddr, addrs(i))
    step(1)
  }

  for (i <- (NumTests - MemReadDelay) until NumTests) {
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
