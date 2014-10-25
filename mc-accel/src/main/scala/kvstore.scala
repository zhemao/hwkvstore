package McAccel

import Chisel._
import Chisel.AdvTester._

class KeyValueStore (
    WordSize: Int, KeySize: Int, NumKeys: Int,
    ValCacheSize: Int, TagSize: Int) extends Module {
  val WordBytes = WordSize / 8
  val CurKeyWords = KeySize / WordBytes
  val AllKeyWords = CurKeyWords * NumKeys
  val HashSize = log2Up(NumKeys)
  val KeyLenSize = log2Up(KeySize)
  val ValAddrSize = log2Up(ValCacheSize)
  val KeyAddrSize = log2Up(AllKeyWords)

  val io = new Bundle {
    val keyInfo = Decoupled(new MessageInfo(KeyLenSize, TagSize)).flip
    val keyData = Decoupled(UInt(width = 8)).flip
    val resultInfo = Decoupled(new MessageInfo(ValAddrSize, TagSize))
    val resultData = Decoupled(UInt(width = 8))
    val rocc = new RoCCInterface
  }

  val lookup = Module(
    new LookupPipeline(WordSize, KeySize, NumKeys, ValCacheSize, TagSize))

  val ctrl = Module(
    new CtrlModule(WordSize, ValAddrSize, KeyLenSize, HashSize, TagSize))
}

class KeyValueStoreTest(c: KeyValueStore) extends AdvTester(c) {
}

object KeyValueStoreMain {
  def main(args: Array[String]) {
    chiselMainTest(args,
      () => Module(new KeyValueStore(32, 256, 16, 1024, 4))) {
        c => new KeyValueStoreTest(c)
    }
  }
}
