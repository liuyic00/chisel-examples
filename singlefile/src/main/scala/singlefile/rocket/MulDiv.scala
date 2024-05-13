/* https://github.com/chipsalliance/rocket-chip/blob/dbcb06afe1c76d1129cb6d264949322a34c37185/src/main/scala/rocket/Multiplier.scala */

package singlefile.rocket.MulDiv

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

object util {
  implicit class BooleanToAugmentedBoolean(private val x: Boolean) extends AnyVal {
    def toInt: Int = if (x) 1 else 0
    // this one's snagged from scalaz
    def option[T](z: => T): Option[T] = if (x) Some(z) else None
  }
}
object DecodeLogic {
  // TODO This should be a method on BitPat
  private def hasDontCare(bp: BitPat): Boolean = bp.mask.bitCount != bp.width
  // Pads BitPats that are safe to pad (no don't cares), errors otherwise
  private def padBP(bp: BitPat, width: Int): BitPat = {
    if (bp.width == width) bp
    else {
      require(!hasDontCare(bp), s"Cannot pad '$bp' to '$width' bits because it has don't cares")
      val diff = width - bp.width
      require(diff > 0, s"Cannot pad '$bp' to '$width' because it is already '${bp.width}' bits wide!")
      BitPat(0.U(diff.W)) ## bp
    }
  }

  def apply(addr: UInt, default: BitPat, mapping: Iterable[(BitPat, BitPat)]): UInt =
    chisel3.util.experimental.decode.decoder(QMCMinimizer, addr, TruthTable(mapping, default))
  def apply(addr: UInt, default: Seq[BitPat], mappingIn: Iterable[(BitPat, Seq[BitPat])]): Seq[UInt] = {
    val nElts = default.size
    require(
      mappingIn.forall(_._2.size == nElts),
      s"All Seq[BitPat] must be of the same length, got $nElts vs. ${mappingIn.find(_._2.size != nElts).get}"
    )

    val elementsGrouped = mappingIn.map(_._2).transpose
    val elementWidths = elementsGrouped.zip(default).map { case (elts, default) =>
      (default :: elts.toList).map(_.getWidth).max
    }
    val resultWidth = elementWidths.sum

    val elementIndices = elementWidths.scan(resultWidth - 1) { case (l, r) => l - r }

    // All BitPats that correspond to a given element in the result must have the same width in the
    // chisel3 decoder. We will zero pad any BitPats that are too small so long as they dont have
    // any don't cares. If there are don't cares, it is an error and the user needs to pad the
    // BitPat themselves
    val defaultsPadded = default.zip(elementWidths).map { case (bp, w) => padBP(bp, w) }
    val mappingInPadded = mappingIn.map { case (in, elts) =>
      in -> elts.zip(elementWidths).map { case (bp, w) => padBP(bp, w) }
    }
    val decoded =
      apply(addr, defaultsPadded.reduce(_ ## _), mappingInPadded.map { case (in, out) => (in, out.reduce(_ ## _)) })

    elementIndices.zip(elementIndices.tail).map { case (msb, lsb) => decoded(msb, lsb + 1) }.toList
  }
  def apply(addr: UInt, default: Seq[BitPat], mappingIn: List[(UInt, Seq[BitPat])]): Seq[UInt] =
    apply(addr, default, mappingIn.map(m => (BitPat(m._1), m._2)).asInstanceOf[Iterable[(BitPat, Seq[BitPat])]])
  def apply(addr: UInt, trues: Iterable[UInt], falses: Iterable[UInt]): Bool =
    apply(
      addr,
      BitPat.dontCare(1),
      trues.map(BitPat(_) -> BitPat("b1")) ++ falses.map(BitPat(_) -> BitPat("b0"))
    ).asBool
}

object ScalarOpConstants {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

  val SZ_DW = 1
  def DW_32 = false.B
}
import ScalarOpConstants._

class ALUFN {
  val SZ_ALU_FN = 4
  def FN_ADD    = 0.U
  def FN_SL     = 1.U
  def FN_SEQ    = 2.U
  def FN_SNE    = 3.U
  def FN_XOR    = 4.U
  def FN_SR     = 5.U
  def FN_OR     = 6.U
  def FN_AND    = 7.U

  // Mul/div reuse some integer FNs
  def FN_DIV  = FN_XOR
  def FN_DIVU = FN_SR
  def FN_REM  = FN_OR
  def FN_REMU = FN_AND

  def FN_MUL    = FN_ADD
  def FN_MULH   = FN_SL
  def FN_MULHSU = FN_SEQ
  def FN_MULHU  = FN_SNE

}

class MultiplierReq(dataBits: Int, tagBits: Int, aluFn: ALUFN = new ALUFN) extends Bundle {
  val fn  = Bits(aluFn.SZ_ALU_FN.W)
  val dw  = Bits(SZ_DW.W)
  val in1 = Bits(dataBits.W)
  val in2 = Bits(dataBits.W)
  val tag = UInt(tagBits.W)
}

class MultiplierResp(dataBits: Int, tagBits: Int) extends Bundle {
  val data = Bits(dataBits.W)
  val tag  = UInt(tagBits.W)
}

class MultiplierIO(val dataBits: Int, val tagBits: Int, aluFn: ALUFN = new ALUFN) extends Bundle {
  val req  = Flipped(Decoupled(new MultiplierReq(dataBits, tagBits, aluFn)))
  val kill = Input(Bool())
  val resp = Decoupled(new MultiplierResp(dataBits, tagBits))
}

case class MulDivParams(
    mulUnroll: Int = 1,
    divUnroll: Int = 1,
    mulEarlyOut: Boolean = false,
    divEarlyOut: Boolean = false,
    divEarlyOutGranularity: Int = 1
)

class MulDiv(cfg: MulDivParams, width: Int, nXpr: Int = 32, aluFn: ALUFN = new ALUFN) extends Module {
  import util._
  private def minDivLatency = (cfg.divUnroll > 0).option(if (cfg.divEarlyOut) 3 else 1 + w / cfg.divUnroll)
  private def minMulLatency = (cfg.mulUnroll > 0).option(if (cfg.mulEarlyOut) 2 else w / cfg.mulUnroll)
  def minLatency: Int       = (minDivLatency ++ minMulLatency).min

  val io       = IO(new MultiplierIO(width, log2Up(nXpr), aluFn))
  val w        = io.req.bits.in1.getWidth
  val mulw     = if (cfg.mulUnroll == 0) w else (w + cfg.mulUnroll - 1) / cfg.mulUnroll * cfg.mulUnroll
  val fastMulW = if (cfg.mulUnroll == 0) false else w / 2 > cfg.mulUnroll && w % (2 * cfg.mulUnroll) == 0

  val s_ready :: s_neg_inputs :: s_mul :: s_div :: s_dummy :: s_neg_output :: s_done_mul :: s_done_div :: Nil = Enum(8)
  val state = RegInit(s_ready)

  val req = Reg(chiselTypeOf(io.req.bits))
  val count = Reg(
    UInt(
      log2Ceil(
        ((cfg.divUnroll != 0).option(w / cfg.divUnroll + 1).toSeq ++
          (cfg.mulUnroll != 0).option(mulw / cfg.mulUnroll)).reduce(_ max _)
      ).W
    )
  )
  val neg_out   = Reg(Bool())
  val isHi      = Reg(Bool())
  val resHi     = Reg(Bool())
  val divisor   = Reg(Bits((w + 1).W))        // div only needs w bits
  val remainder = Reg(Bits((2 * mulw + 2).W)) // div only needs 2*w+1 bits

  val mulDecode = List(
    aluFn.FN_MUL    -> List(Y, N, X, X),
    aluFn.FN_MULH   -> List(Y, Y, Y, Y),
    aluFn.FN_MULHU  -> List(Y, Y, N, N),
    aluFn.FN_MULHSU -> List(Y, Y, Y, N)
  )
  val divDecode = List(
    aluFn.FN_DIV  -> List(N, N, Y, Y),
    aluFn.FN_REM  -> List(N, Y, Y, Y),
    aluFn.FN_DIVU -> List(N, N, N, N),
    aluFn.FN_REMU -> List(N, Y, N, N)
  )
  val cmdMul :: cmdHi :: lhsSigned :: rhsSigned :: Nil =
    DecodeLogic(
      io.req.bits.fn,
      List(X, X, X, X),
      (if (cfg.divUnroll != 0) divDecode else Nil) ++ (if (cfg.mulUnroll != 0) mulDecode else Nil)
    ).map(_.asBool)

  require(w == 32 || w == 64)
  def halfWidth(req: MultiplierReq) = (w > 32).B && req.dw === DW_32

  def sext(x: Bits, halfW: Bool, signed: Bool) = {
    val sign = signed && Mux(halfW, x(w / 2 - 1), x(w - 1))
    val hi   = Mux(halfW, Fill(w / 2, sign), x(w - 1, w / 2))
    (Cat(hi, x(w / 2 - 1, 0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, halfWidth(io.req.bits), lhsSigned)
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, halfWidth(io.req.bits), rhsSigned)

  val subtractor        = remainder(2 * w, w) - divisor
  val result            = Mux(resHi, remainder(2 * w, w + 1), remainder(w - 1, 0))
  val negated_remainder = -result

  if (cfg.divUnroll != 0) when(state === s_neg_inputs) {
    when(remainder(w - 1)) {
      remainder := negated_remainder
    }
    when(divisor(w - 1)) {
      divisor := subtractor
    }
    state := s_div
  }
  if (cfg.divUnroll != 0) when(state === s_neg_output) {
    remainder := negated_remainder
    state     := s_done_div
    resHi     := false.B
  }
  if (cfg.mulUnroll != 0) when(state === s_mul) {
    val mulReg         = Cat(remainder(2 * mulw + 1, w + 1), remainder(w - 1, 0))
    val mplierSign     = remainder(w)
    val mplier         = mulReg(mulw - 1, 0)
    val accum          = mulReg(2 * mulw, mulw).asSInt
    val mpcand         = divisor.asSInt
    val prod           = Cat(mplierSign, mplier(cfg.mulUnroll - 1, 0)).asSInt * mpcand + accum
    val nextMulReg     = Cat(prod, mplier(mulw - 1, cfg.mulUnroll))
    val nextMplierSign = count === (mulw / cfg.mulUnroll - 2).U && neg_out

    val eOutMask = ((BigInt(-1) << mulw).S >> (count * cfg.mulUnroll.U)(log2Up(mulw) - 1, 0))(mulw - 1, 0)
    val eOut = (cfg.mulEarlyOut).B && count =/= (mulw / cfg.mulUnroll - 1).U && count =/= 0.U &&
      !isHi && (mplier & ~eOutMask) === 0.U
    val eOutRes     = (mulReg >> (mulw.U - count * cfg.mulUnroll.U)(log2Up(mulw) - 1, 0))
    val nextMulReg1 = Cat(nextMulReg(2 * mulw, mulw), Mux(eOut, eOutRes, nextMulReg)(mulw - 1, 0))
    remainder := Cat(nextMulReg1 >> w, nextMplierSign, nextMulReg1(w - 1, 0))

    count := count + 1.U
    when(eOut || count === (mulw / cfg.mulUnroll - 1).U) {
      state := s_done_mul
      resHi := isHi
    }
  }
  if (cfg.divUnroll != 0) when(state === s_div) {
    val unrolls = ((0 until cfg.divUnroll) scanLeft remainder) { case (rem, i) =>
      // the special case for iteration 0 is to save HW, not for correctness
      val difference = if (i == 0) subtractor else rem(2 * w, w) - divisor(w - 1, 0)
      val less       = difference(w)
      Cat(Mux(less, rem(2 * w - 1, w), difference(w - 1, 0)), rem(w - 1, 0), !less)
    }.tail

    remainder := unrolls.last
    when(count === (w / cfg.divUnroll).U) {
      state       := Mux(neg_out, s_neg_output, s_done_div)
      resHi       := isHi
      if (w        % cfg.divUnroll < cfg.divUnroll - 1)
        remainder := unrolls(w % cfg.divUnroll)
    }
    count := count + 1.U

    val divby0 = count === 0.U && !subtractor(w)
    if (cfg.divEarlyOut) {
      val align       = 1 << log2Floor(cfg.divUnroll max cfg.divEarlyOutGranularity)
      val alignMask   = ~((align - 1).U(log2Ceil(w).W))
      val divisorMSB  = Log2(divisor(w - 1, 0), w) & alignMask
      val dividendMSB = Log2(remainder(w - 1, 0), w) | ~alignMask
      val eOutPos     = ~(dividendMSB - divisorMSB)
      val eOut        = count === 0.U && !divby0 && eOutPos >= align.U
      when(eOut) {
        remainder := remainder(w - 1, 0) << eOutPos
        count     := eOutPos >> log2Floor(cfg.divUnroll)
      }
    }
    when(divby0 && !isHi) { neg_out := false.B }
  }
  when(io.resp.fire || io.kill) {
    state := s_ready
  }
  when(io.req.fire) {
    state     := Mux(cmdMul, s_mul, Mux(lhs_sign || rhs_sign, s_neg_inputs, s_div))
    isHi      := cmdHi
    resHi     := false.B
    count     := (if (fastMulW) Mux[UInt](cmdMul && halfWidth(io.req.bits), (w / cfg.mulUnroll / 2).U, 0.U) else 0.U)
    neg_out   := Mux(cmdHi, lhs_sign, lhs_sign =/= rhs_sign)
    divisor   := Cat(rhs_sign, rhs_in)
    remainder := lhs_in
    req       := io.req.bits
  }

  val outMul = (state & (s_done_mul ^ s_done_div)) === (s_done_mul & ~s_done_div)
  val loOut  = Mux(fastMulW.B && halfWidth(req) && outMul, result(w - 1, w / 2), result(w / 2 - 1, 0))
  val hiOut  = Mux(halfWidth(req), Fill(w / 2, loOut(w / 2 - 1)), result(w - 1, w / 2))
  io.resp.bits.tag := req.tag

  io.resp.bits.data := Cat(hiOut, loOut)
  io.resp.valid     := (state === s_done_mul || state === s_done_div)
  io.req.ready      := state === s_ready
}
