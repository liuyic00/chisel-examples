/* https://github.com/OSCPU/NutShell/blob/master/src/main/scala/nutcore/mem/Cache.scala */

package singlefile.nutshell.cache

import chisel3._
import chisel3.util._

/* src/main/scala/utils/BitUtils.scala */

object MaskExpand {
  def apply(m: UInt) = Cat(m.asBools.map(Fill(8, _)).reverse)
}

/* src/main/scala/bus/simplebus/SimpleBus.scala */

// Uncache
class SimpleBusUC(val userBits: Int = 0, val addrBits: Int = 32, val idBits: Int = 0) extends SimpleBusBundle {
  val req  = Decoupled(new SimpleBusReqBundle(userBits, addrBits, idBits))
  val resp = Flipped(Decoupled(new SimpleBusRespBundle(userBits, idBits)))

  def isWrite = req.valid && req.bits.isWrite()
  def isRead  = req.valid && req.bits.isRead()
}

object SimpleBusCmd {
  // req
  //                              hit    |    miss
  def read       = "b0000".U //  read    |   refill
  def write      = "b0001".U //  write   |   refill
  def readBurst  = "b0010".U //  read    |   refill
  def writeBurst = "b0011".U //  write   |   refill
  def writeLast  = "b0111".U //  write   |   refill
  def probe      = "b1000".U //  read    | do nothing
  def prefetch   = "b0100".U //  read    |   refill

  // resp
  def readLast  = "b0110".U
  def writeResp = "b0101".U
  def probeHit  = "b1100".U
  def probeMiss = "b1000".U

  def apply() = UInt(4.W)
}

sealed abstract class SimpleBusBundle extends Bundle with HasNutCoreParameter

class SimpleBusReqBundle(val userBits: Int = 0, val addrBits: Int = 32, val idBits: Int = 0) extends SimpleBusBundle {
  val addr  = Output(UInt(addrBits.W))
  val size  = Output(UInt(3.W))
  val cmd   = Output(SimpleBusCmd())
  val wmask = Output(UInt((DataBits / 8).W))
  val wdata = Output(UInt(DataBits.W))
  val user  = if (userBits > 0) Some(Output(UInt(userBits.W))) else None
  val id    = if (idBits > 0) Some(Output(UInt(idBits.W))) else None

  def apply(addr: UInt, cmd: UInt, size: UInt, wdata: UInt, wmask: UInt, user: UInt = 0.U, id: UInt = 0.U) = {
    this.addr  := addr
    this.cmd   := cmd
    this.size  := size
    this.wdata := wdata
    this.wmask := wmask
    this.user.map(_ := user)
    this.id.map(_ := id)
  }

  def isRead()      = !cmd(0) && !cmd(3)
  def isWrite()     = cmd(0)
  def isBurst()     = cmd(1)
  def isReadBurst() = cmd === SimpleBusCmd.readBurst
  def isWriteLast() = cmd === SimpleBusCmd.writeLast
  def isProbe()     = cmd === SimpleBusCmd.probe
  def isPrefetch()  = cmd === SimpleBusCmd.prefetch
}

class SimpleBusRespBundle(val userBits: Int = 0, val idBits: Int = 0) extends SimpleBusBundle {
  val cmd   = Output(SimpleBusCmd())
  val rdata = Output(UInt(64.W)) // TODO: when frontend datapath support 32bit, set DataBits.W here
  val user  = if (userBits > 0) Some(Output(UInt(userBits.W))) else None
  val id    = if (idBits > 0) Some(Output(UInt(idBits.W))) else None

  def isReadLast  = cmd === SimpleBusCmd.readLast
  def isProbeHit  = cmd === SimpleBusCmd.probeHit
  def isProbeMiss = cmd === SimpleBusCmd.probeMiss
  def isWriteResp = cmd === SimpleBusCmd.writeResp
  def isPrefetch  = cmd === SimpleBusCmd.prefetch
}

// Cache
class SimpleBusC(val userBits: Int = 0) extends SimpleBusBundle {
  val mem = new SimpleBusUC(userBits)
  val coh = Flipped(new SimpleBusUC(userBits))
}

/* src/main/scala/nutcore/NutCore.scala */

trait HasNutCoreParameter {
  // General Parameter for NutShell
  val XLEN      = if (Settings.get("IsRV32")) 32 else 64
  val HasIcache = Settings.get("HasIcache")
  val AddrBits  = 64 // AddrBits is used in some cases
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val DataBits  = XLEN
  val DataBytes = DataBits / 8
}

/* src/main/scala/top/Settings.scala */

object DefaultSettings {
  def apply() = Map(
    "MMIOBase"    -> 0x0000000040000000L,
    "MMIOSize"    -> 0x0000000040000000L,
    "HasPrefetch" -> true,
    "IsRV32"      -> false
  )
}

object Settings {
  var settings: Map[String, AnyVal] = DefaultSettings()
  def get(field: String) = {
    settings(field).asInstanceOf[Boolean]
  }
  def getLong(field: String) = {
    settings(field).asInstanceOf[Long]
  }
}

/* src/main/scala/utils/Hold.scala */

object HoldUnless {
  def apply[T <: Data](x: T, en: Bool): T = Mux(en, x, RegEnable(x, 0.U.asTypeOf(x), en))
}

object ReadAndHold {
  def apply[T <: Data](x: Mem[T], addr: UInt, en: Bool): T         = HoldUnless(x.read(addr), en)
  def apply[T <: Data](x: SyncReadMem[T], addr: UInt, en: Bool): T = HoldUnless(x.read(addr, en), RegNext(en))
}

/* src/main/scala/utils/SRAMTemplate.scala */

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req  = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data    = Output(gen)
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: T, setIdx: UInt, waymask: UInt) = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt) = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
}

class SRAMTemplate[T <: Data](
    gen: T,
    set: Int,
    way: Int = 1,
    shouldReset: Boolean = false,
    holdRead: Boolean = false,
    singlePort: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType               = UInt(gen.getWidth.W)
  val array                  = SyncReadMem(set, Vec(way, wordType))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState              = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when(resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet   := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen    = (if (singlePort) ren && !wen else ren)

  val setIdx    = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask   = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  val wdata     = VecInit(Seq.fill(way)(wdataword))
  when(wen) { array.write(setIdx, wdata, waymask.asBools) }

  val rdata = (if (holdRead) ReadAndHold(array, io.r.req.bits.setIdx, realRen)
               else array.read(io.r.req.bits.setIdx, realRen)).map(_.asTypeOf(gen))
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1, shouldReset: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map {
    case r => {
      r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire))
    }
  }
}

/* src/main/scala/utils/Pipeline.scala */

object PipelineConnect {
  def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool) = {
    val valid = RegInit(false.B)
    when(rightOutFire) { valid := false.B }
    when(left.valid && right.ready) { valid := true.B }
    when(isFlush) { valid := false.B }

    left.ready  := right.ready
    right.bits  := RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid // && !isFlush
  }
}

/* src/main/scala/utils/BitUtils.scala */

object MaskData {
  def apply(oldData: UInt, newData: UInt, fullmask: UInt) = {
    require(oldData.getWidth == newData.getWidth)
    require(oldData.getWidth == fullmask.getWidth)
    (newData & fullmask) | (oldData & ~fullmask)
  }
}

/* src/main/scala/nutcore/NutCore.scala */

trait HasNutCoreLog { this: RawModule =>
  implicit val moduleName: String = this.name
}

object AddressSpace extends HasNutCoreParameter {
  // (start, size)
  // address out of MMIO will be considered as DRAM
  def mmio = List(
    (0x30000000L, 0x10000000L),                                  // internal devices, such as CLINT and PLIC
    (Settings.getLong("MMIOBase"), Settings.getLong("MMIOSize")) // external devices
  )

  def isMMIO(addr: UInt) = mmio
    .map(range => {
      require(isPow2(range._2))
      val bits = log2Up(range._2)
      (addr ^ range._1.U)(PAddrBits - 1, bits) === 0.U
    })
    .reduce(_ || _)
}

/* src/main/scala/utils/LFSR64.scala */

object LFSR64 {
  def apply(increment: Bool = true.B): UInt = {
    val wide = 64
    val lfsr = RegInit(0x1234567887654321L.U(wide.W)) // random initial value based on simulation seed
    val xor  = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when(increment) {
      lfsr := Mux(lfsr === 0.U, 1.U, Cat(xor, lfsr(wide - 1, 1)))
    }
    lfsr
  }
}

/* Cache.scala */

case class CacheConfig(
    ro: Boolean = false,
    name: String = "cache",
    userBits: Int = 0,
    idBits: Int = 0,
    cacheLevel: Int = 1,
    totalSize: Int = 32, // Kbytes
    ways: Int = 4
)

sealed trait HasCacheConst {
  implicit val cacheConfig: CacheConfig

  val PAddrBits: Int
  val XLEN: Int

  val cacheName = cacheConfig.name
  val userBits  = cacheConfig.userBits
  val idBits    = cacheConfig.idBits

  val ro          = cacheConfig.ro
  val hasCoh      = !ro
  val hasCohInt   = (if (hasCoh) 1 else 0)
  val hasPrefetch = cacheName == "l2cache"

  val cacheLevel    = cacheConfig.cacheLevel
  val TotalSize     = cacheConfig.totalSize
  val Ways          = cacheConfig.ways
  val LineSize      = XLEN         // byte
  val LineBeats     = LineSize / 8 // DATA WIDTH 64
  val Sets          = TotalSize * 1024 / LineSize / Ways
  val OffsetBits    = log2Up(LineSize)
  val IndexBits     = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits       = PAddrBits - OffsetBits - IndexBits

  val debug = false

  def addrBundle = new Bundle {
    val tag        = UInt(TagBits.W)
    val index      = UInt(IndexBits.W)
    val wordIndex  = UInt(WordIndexBits.W)
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W)
  }

  def CacheMetaArrayReadBus()  = new SRAMReadBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus()  = new SRAMReadBus(new DataBundle, set = Sets * LineBeats, way = Ways)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new DataBundle, set = Sets * LineBeats, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(addr.asTypeOf(addrBundle).index, addr.asTypeOf(addrBundle).wordIndex)

  def isSameWord(a1: UInt, a2: UInt)    = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class CacheBundle(implicit cacheConfig: CacheConfig)
    extends Bundle
    with HasNutCoreParameter
    with HasCacheConst
sealed abstract class CacheModule(implicit cacheConfig: CacheConfig)
    extends Module
    with HasNutCoreParameter
    with HasCacheConst
    with HasNutCoreLog

sealed class MetaBundle(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val tag   = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())

  def apply(tag: UInt, valid: Bool, dirty: Bool) = {
    this.tag   := tag
    this.valid := valid
    this.dirty := dirty
    this
  }
}

sealed class DataBundle(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

sealed class Stage1IO(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
}

class CacheIO(implicit val cacheConfig: CacheConfig) extends Bundle with HasNutCoreParameter with HasCacheConst {
  val in    = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
  val flush = Input(UInt(2.W))
  val out   = new SimpleBusC
  val mmio  = new SimpleBusUC
  val empty = Output(Bool())
}
trait HasCacheIO {
  implicit val cacheConfig: CacheConfig
  val io = IO(new CacheIO)
}

// meta read
sealed class CacheStage1(implicit val cacheConfig: CacheConfig) extends CacheModule {
  class CacheStage1IO extends Bundle {
    val in          = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits, idBits = idBits)))
    val out         = Decoupled(new Stage1IO)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()
  }
  val io = IO(new CacheStage1IO)

  if (ro) when(io.in.fire) { assert(!io.in.bits.isWrite()) }

  // read meta array and data array
  val readBusValid = io.in.valid && io.out.ready
  io.metaReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  io.dataReadBus.apply(valid = readBusValid, setIdx = getDataIdx(io.in.bits.addr))

  io.out.bits.req := io.in.bits
  io.out.valid    := io.in.valid && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.in.ready     := (!io.in.valid || io.out.fire) && io.metaReadBus.req.ready && io.dataReadBus.req.ready

}

sealed class Stage2IO(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val req           = new SimpleBusReqBundle(userBits = userBits, idBits = idBits)
  val metas         = Vec(Ways, new MetaBundle)
  val datas         = Vec(Ways, new DataBundle)
  val hit           = Output(Bool())
  val waymask       = Output(UInt(Ways.W))
  val mmio          = Output(Bool())
  val isForwardData = Output(Bool())
  val forwardData   = Output(CacheDataArrayWriteBus().req.bits)
}

// check
sealed class CacheStage2(implicit val cacheConfig: CacheConfig) extends CacheModule {
  class CacheStage2IO extends Bundle {
    val in           = Flipped(Decoupled(new Stage1IO))
    val out          = Decoupled(new Stage2IO)
    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new DataBundle))
    val metaWriteBus = Input(CacheMetaArrayWriteBus())
    val dataWriteBus = Input(CacheDataArrayWriteBus())
  }
  val io = IO(new CacheStage2IO)

  val req  = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)

  val isForwardMeta =
    io.in.valid && io.metaWriteBus.req.valid && io.metaWriteBus.req.bits.setIdx === getMetaIdx(req.addr)
  val isForwardMetaReg = RegInit(false.B)
  when(isForwardMeta) { isForwardMetaReg := true.B }
  when(io.in.fire || !io.in.valid) { isForwardMetaReg := false.B }
  val forwardMetaReg = RegEnable(io.metaWriteBus.req.bits, isForwardMeta)

  val metaWay         = Wire(Vec(Ways, chiselTypeOf(forwardMetaReg.data)))
  val pickForwardMeta = isForwardMetaReg || isForwardMeta
  val forwardMeta     = Mux(isForwardMeta, io.metaWriteBus.req.bits, forwardMetaReg)
  val forwardWaymask  = forwardMeta.waymask.getOrElse("1".U).asBools
  forwardWaymask.zipWithIndex.map { case (w, i) =>
    metaWay(i) := Mux(pickForwardMeta && w, forwardMeta.data, io.metaReadResp(i))
  }

  val hitVec        = VecInit(metaWay.map(m => m.valid && (m.tag === addr.tag) && io.in.valid)).asUInt
  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways) - 1, 0)) else "b1".U

  val invalidVec    = VecInit(metaWay.map(m => !m.valid)).asUInt
  val hasInvalidWay = invalidVec.orR
  val refillInvalidWaymask =
    Mux(invalidVec >= 8.U, "b1000".U, Mux(invalidVec >= 4.U, "b0100".U, Mux(invalidVec >= 2.U, "b0010".U, "b0001".U)))

  // val waymask = Mux(io.out.bits.hit, hitVec, victimWaymask)
  val waymask = Mux(io.out.bits.hit, hitVec, Mux(hasInvalidWay, refillInvalidWaymask, victimWaymask))
  assert(!(io.in.valid && PopCount(waymask) > 1.U))

  io.out.bits.metas   := metaWay
  io.out.bits.hit     := io.in.valid && hitVec.orR
  io.out.bits.waymask := waymask
  io.out.bits.datas   := io.dataReadResp
  io.out.bits.mmio    := AddressSpace.isMMIO(req.addr)

  val isForwardData = io.in.valid && (io.dataWriteBus.req match {
    case r =>
      r.valid && r.bits.setIdx === getDataIdx(req.addr)
  })
  val isForwardDataReg = RegInit(false.B)
  when(isForwardData) { isForwardDataReg := true.B }
  when(io.in.fire || !io.in.valid) { isForwardDataReg := false.B }
  val forwardDataReg = RegEnable(io.dataWriteBus.req.bits, isForwardData)
  io.out.bits.isForwardData := isForwardDataReg || isForwardData
  io.out.bits.forwardData   := Mux(isForwardData, io.dataWriteBus.req.bits, forwardDataReg)

  io.out.bits.req <> req
  io.out.valid := io.in.valid
  io.in.ready  := !io.in.valid || io.out.fire

}

// writeback
sealed class CacheStage3(implicit val cacheConfig: CacheConfig) extends CacheModule {
  class CacheStage3IO extends Bundle {
    val in           = Flipped(Decoupled(new Stage2IO))
    val out          = Decoupled(new SimpleBusRespBundle(userBits = userBits, idBits = idBits))
    val isFinish     = Output(Bool())
    val flush        = Input(Bool())
    val dataReadBus  = CacheDataArrayReadBus()
    val dataWriteBus = CacheDataArrayWriteBus()
    val metaWriteBus = CacheMetaArrayWriteBus()

    val mem     = new SimpleBusUC
    val mmio    = new SimpleBusUC
    val cohResp = Decoupled(new SimpleBusRespBundle)

    // use to distinguish prefetch request and normal request
    val dataReadRespToL1 = Output(Bool())
  }
  val io = IO(new CacheStage3IO)

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))

  val req          = io.in.bits.req
  val addr         = req.addr.asTypeOf(addrBundle)
  val mmio         = io.in.valid && io.in.bits.mmio
  val hit          = io.in.valid && io.in.bits.hit
  val miss         = io.in.valid && !io.in.bits.hit
  val probe        = io.in.valid && hasCoh.B && req.isProbe()
  val hitReadBurst = hit && req.isReadBurst()
  val meta         = Mux1H(io.in.bits.waymask, io.in.bits.metas)
  assert(!(mmio && hit), "MMIO request should not hit in cache")

  val useForwardData =
    io.in.bits.isForwardData && io.in.bits.waymask === io.in.bits.forwardData.waymask.getOrElse("b1".U)
  val dataReadArray = Mux1H(io.in.bits.waymask, io.in.bits.datas).data
  val dataRead      = Mux(useForwardData, io.in.bits.forwardData.data.data, dataReadArray)
  val wordMask      = Mux(!ro.B && req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))

  val writeL2BeatCnt = Counter(LineBeats)
  when(io.out.fire && (req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast())) {
    writeL2BeatCnt.inc()
  }

  val hitWrite = hit && req.isWrite()
  val dataHitWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data = Wire(new DataBundle).apply(MaskData(dataRead, req.wdata, wordMask)),
    valid = hitWrite,
    setIdx = Cat(
      addr.index,
      Mux(req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast(), writeL2BeatCnt.value, addr.wordIndex)
    ),
    waymask = io.in.bits.waymask
  )

  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hitWrite && !meta.dirty,
    setIdx = getMetaIdx(req.addr),
    waymask = io.in.bits.waymask,
    data = Wire(new MetaBundle).apply(tag = meta.tag, valid = true.B, dirty = (!ro).B)
  )

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_mmioReq :: s_mmioResp :: s_wait_resp :: s_release :: Nil =
    Enum(9)
  val state     = RegInit(s_idle)
  val needFlush = RegInit(false.B)

  when(io.flush && (state =/= s_idle)) { needFlush := true.B }
  when(io.out.fire && needFlush) { needFlush := false.B }

  val readBeatCnt  = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)

  val s2_idle :: s2_dataReadWait :: s2_dataOK :: Nil = Enum(3)
  val state2                                         = RegInit(s2_idle)

  io.dataReadBus.apply(
    valid = (state === s_memWriteReq || state === s_release) && (state2 === s2_idle),
    setIdx = Cat(addr.index, Mux(state === s_release, readBeatCnt.value, writeBeatCnt.value))
  )
  val dataWay    = RegEnable(io.dataReadBus.resp.data, state2 === s2_dataReadWait)
  val dataHitWay = Mux1H(io.in.bits.waymask, dataWay).data

  switch(state2) {
    is(s2_idle) { when(io.dataReadBus.req.fire) { state2 := s2_dataReadWait } }
    is(s2_dataReadWait) { state2 := s2_dataOK }
    is(s2_dataOK) { when(io.mem.req.fire || io.cohResp.fire || hitReadBurst && io.out.ready) { state2 := s2_idle } }
  }

  // critical word first read
  val raddr =
    (if (XLEN == 64) Cat(req.addr(PAddrBits - 1, 3), 0.U(3.W))
     else Cat(req.addr(PAddrBits - 1, 2), 0.U(2.W)))
  // dirty block addr
  val waddr = Cat(meta.tag, addr.index, 0.U(OffsetBits.W))
  val cmd = Mux(
    state === s_memReadReq,
    SimpleBusCmd.readBurst,
    Mux((writeBeatCnt.value === (LineBeats - 1).U), SimpleBusCmd.writeLast, SimpleBusCmd.writeBurst)
  )
  io.mem.req.bits.apply(
    addr = Mux(state === s_memReadReq, raddr, waddr),
    cmd = cmd,
    size = (if (XLEN == 64) "b11".U else "b10".U),
    wdata = dataHitWay,
    wmask = Fill(DataBytes, 1.U)
  )

  io.mem.resp.ready := true.B
  io.mem.req.valid  := (state === s_memReadReq) || ((state === s_memWriteReq) && (state2 === s2_dataOK))

  // mmio
  io.mmio.req.bits   := req
  io.mmio.resp.ready := true.B
  io.mmio.req.valid  := (state === s_mmioReq)

  val afterFirstRead = RegInit(false.B)
  val alreadyOutFire = RegEnable(true.B, false.B, io.out.fire)
  val readingFirst   = !afterFirstRead && io.mem.resp.fire && (state === s_memReadResp)
  val inRdataRegDemand =
    RegEnable(Mux(mmio, io.mmio.resp.bits.rdata, io.mem.resp.bits.rdata), Mux(mmio, state === s_mmioResp, readingFirst))

  // probe
  io.cohResp.valid := ((state === s_idle) && probe) ||
    ((state === s_release) && (state2 === s2_dataOK))
  io.cohResp.bits.rdata := dataHitWay
  val releaseLast = Counter(state === s_release && io.cohResp.fire, LineBeats)._2
  io.cohResp.bits.cmd := Mux(
    state === s_release,
    Mux(releaseLast, SimpleBusCmd.readLast, 0.U),
    Mux(hit, SimpleBusCmd.probeHit, SimpleBusCmd.probeMiss)
  )

  val respToL1Fire = hitReadBurst && io.out.ready && state2 === s2_dataOK
  val respToL1Last = Counter(
    (state === s_idle || state === s_release && state2 === s2_dataOK) && hitReadBurst && io.out.ready,
    LineBeats
  )._2

  switch(state) {
    is(s_idle) {
      afterFirstRead := false.B
      alreadyOutFire := false.B

      when(probe) {
        when(io.cohResp.fire) {
          state             := Mux(hit, s_release, s_idle)
          readBeatCnt.value := addr.wordIndex
        }
      }.elsewhen(hitReadBurst && io.out.ready) {
        state             := s_release
        readBeatCnt.value := Mux(addr.wordIndex === (LineBeats - 1).U, 0.U, (addr.wordIndex + 1.U))
      }.elsewhen((miss || mmio) && !io.flush) {
        state := Mux(mmio, s_mmioReq, Mux(!ro.B && meta.dirty, s_memWriteReq, s_memReadReq))
      }
    }

    is(s_mmioReq) { when(io.mmio.req.fire) { state := s_mmioResp } }
    is(s_mmioResp) { when(io.mmio.resp.fire) { state := s_wait_resp } }

    is(s_release) {
      when(io.cohResp.fire || respToL1Fire) { readBeatCnt.inc() }
      when(probe && io.cohResp.fire && releaseLast || respToL1Fire && respToL1Last) { state := s_idle }
    }

    is(s_memReadReq) {
      when(io.mem.req.fire) {
        state             := s_memReadResp
        readBeatCnt.value := addr.wordIndex
      }
    }

    is(s_memReadResp) {
      when(io.mem.resp.fire) {
        afterFirstRead := true.B
        readBeatCnt.inc()
        when(req.cmd === SimpleBusCmd.writeBurst) { writeL2BeatCnt.value := 0.U }
        when(io.mem.resp.bits.isReadLast) { state := s_wait_resp }
      }
    }

    is(s_memWriteReq) {
      when(io.mem.req.fire) { writeBeatCnt.inc() }
      when(io.mem.req.bits.isWriteLast() && io.mem.req.fire) { state := s_memWriteResp }
    }

    is(s_memWriteResp) { when(io.mem.resp.fire) { state := s_memReadReq } }
    is(s_wait_resp) { when(io.out.fire || needFlush || alreadyOutFire) { state := s_idle } }
  }

  val dataRefill = MaskData(io.mem.resp.bits.rdata, req.wdata, Mux(readingFirst, wordMask, 0.U(DataBits.W)))
  val dataRefillWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire,
    setIdx = Cat(addr.index, readBeatCnt.value),
    data = Wire(new DataBundle).apply(dataRefill),
    waymask = io.in.bits.waymask
  )

  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  val metaRefillWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire && io.mem.resp.bits.isReadLast,
    data = Wire(new MetaBundle).apply(valid = true.B, tag = addr.tag, dirty = !ro.B && req.isWrite()),
    setIdx = getMetaIdx(req.addr),
    waymask = io.in.bits.waymask
  )

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  if (cacheLevel == 2) {
    when((state === s_memReadResp) && io.mem.resp.fire && req.isReadBurst()) {
      // readBurst request miss
      io.out.bits.rdata := dataRefill
      io.out.bits.cmd   := Mux(io.mem.resp.bits.isReadLast, SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
    }.elsewhen(req.isWriteLast() || req.cmd === SimpleBusCmd.writeBurst) {
      // writeBurst/writeLast request, no matter hit or miss
      io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
      io.out.bits.cmd   := DontCare
    }.elsewhen(hitReadBurst && state === s_release) {
      // readBurst request hit
      io.out.bits.rdata := dataHitWay
      io.out.bits.cmd   := Mux(respToL1Last, SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
    }.otherwise {
      io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
      io.out.bits.cmd   := req.cmd
    }
  } else {
    io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
    io.out.bits.cmd := Mux(
      io.in.bits.req.isRead(),
      SimpleBusCmd.readLast,
      Mux(io.in.bits.req.isWrite(), SimpleBusCmd.writeResp, DontCare)
    ) // DontCare, added by lemover
  }
  io.out.bits.user.zip(req.user).map { case (o, i) => o := i }
  io.out.bits.id.zip(req.id).map { case (o, i) => o := i }

  io.out.valid := io.in.valid && Mux(
    req.isBurst() && (cacheLevel == 2).B,
    Mux(
      req.isWrite() && (hit || !hit && state === s_wait_resp),
      true.B,
      (state === s_memReadResp && io.mem.resp.fire && req.cmd === SimpleBusCmd.readBurst)
    ) || (respToL1Fire && respToL1Last && state === s_release),
    Mux(
      probe,
      false.B,
      Mux(hit, true.B, Mux(req.isWrite() || mmio, state === s_wait_resp, afterFirstRead && !alreadyOutFire))
    )
  )

  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.
  io.isFinish := Mux(
    probe,
    io.cohResp.fire && Mux(miss, state === s_idle, (state === s_release) && releaseLast),
    Mux(hit || req.isWrite(), io.out.fire, (state === s_wait_resp) && (io.out.fire || alreadyOutFire))
  )

  io.in.ready := io.out.ready && (state === s_idle && !hitReadBurst) && !miss && !probe
  io.dataReadRespToL1 := hitReadBurst && (state === s_idle && io.out.ready || state === s_release && state2 === s2_dataOK)

  assert(!(metaHitWriteBus.req.valid && metaRefillWriteBus.req.valid))
  assert(!(dataHitWriteBus.req.valid && dataRefillWriteBus.req.valid))
  assert(!(!ro.B && io.flush), "only allow to flush icache")
}

class Cache(implicit val cacheConfig: CacheConfig) extends CacheModule with HasCacheIO {
  // cpu pipeline
  val s1 = Module(new CacheStage1)
  val s2 = Module(new CacheStage2)
  val s3 = Module(new CacheStage3)
  val metaArray = Module(
    new SRAMTemplateWithArbiter(nRead = 1, new MetaBundle, set = Sets, way = Ways, shouldReset = true)
  )
  val dataArray = Module(new SRAMTemplateWithArbiter(nRead = 2, new DataBundle, set = Sets * LineBeats, way = Ways))

  val arb = Module(new Arbiter(new SimpleBusReqBundle(userBits = userBits, idBits = idBits), hasCohInt + 1))
  arb.io.in(hasCohInt + 0) <> io.in.req

  s1.io.in <> arb.io.out
  /*
  val s2BlockByPrefetch = if (cacheLevel == 2) {
      s2.io.out.valid && s3.io.in.valid && s3.io.in.bits.req.isPrefetch() && !s3.io.in.ready
    } else { false.B }
   */
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire, io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1))
  io.in.resp <> s3.io.out
  s3.io.flush := io.flush(1)
  io.out.mem <> s3.io.mem
  io.mmio <> s3.io.mmio
  io.empty := !s2.io.in.valid && !s3.io.in.valid

  io.in.resp.valid := Mux(
    s3.io.out.valid && s3.io.out.bits.isPrefetch,
    false.B,
    s3.io.out.valid || s3.io.dataReadRespToL1
  )

  if (hasCoh) {
    val cohReq = io.out.coh.req.bits
    // coh does not have user signal, any better code?
    val coh = Wire(new SimpleBusReqBundle(userBits = userBits, idBits = idBits))
    coh.apply(addr = cohReq.addr, cmd = cohReq.cmd, size = cohReq.size, wdata = cohReq.wdata, wmask = cohReq.wmask)
    arb.io.in(0).bits    := coh
    arb.io.in(0).valid   := io.out.coh.req.valid
    io.out.coh.req.ready := arb.io.in(0).ready
    io.out.coh.resp <> s3.io.cohResp
  } else {
    io.out.coh.req.ready  := true.B
    io.out.coh.resp       := DontCare
    io.out.coh.resp.valid := false.B
    s3.io.cohResp.ready   := true.B
  }

  metaArray.io.r(0) <> s1.io.metaReadBus
  dataArray.io.r(0) <> s1.io.dataReadBus
  dataArray.io.r(1) <> s3.io.dataReadBus

  metaArray.io.w <> s3.io.metaWriteBus
  dataArray.io.w <> s3.io.dataWriteBus

  s2.io.metaReadResp := s1.io.metaReadBus.resp.data
  s2.io.dataReadResp := s1.io.dataReadBus.resp.data
  s2.io.dataWriteBus := s3.io.dataWriteBus
  s2.io.metaWriteBus := s3.io.metaWriteBus

}
