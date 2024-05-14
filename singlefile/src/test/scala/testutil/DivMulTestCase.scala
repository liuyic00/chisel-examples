package testutil

import scala.util.Random

/** a * b = c
  */
case class MulTestCase(a: BigInt, b: BigInt, c: BigInt, width: Int, aSigned: Boolean, bSigned: Boolean) {
  val aBits = if (aSigned) ToSIntBits(a, width) else a
  val bBits = if (bSigned) ToSIntBits(b, width) else b
  val cBits = if (aSigned || bSigned) ToSIntBits(c, width * 2) else c

  val xaBits = if (aSigned) sigExt(aBits, width, width + 1) else aBits
  val xbBits = if (bSigned) sigExt(bBits, width, width + 1) else bBits
  val xcBits = if (aSigned || bSigned) sigExt(cBits, width * 2, width * 2 + 2) else cBits

  def sigExt(v: BigInt, w: Int, nw: Int): BigInt = {
    val sigBit = v & (BigInt(1) << (w - 1))
    if (sigBit == BigInt(0)) v
    else v | (((BigInt(1) << (nw - w)) - 1) << w)
  }
}

object MulTestCase {
  def random(width: Int, aSigned: Boolean, bSigned: Boolean): MulTestCase = {
    val a = if (aSigned) RandomBigInt.signed(width) else RandomBigInt(width)
    val b = if (bSigned) RandomBigInt.signed(width) else RandomBigInt(width)
    val c = a * b
    MulTestCase(a, b, c, width, aSigned, bSigned)
  }
  def random(width: Int): MulTestCase = {
    val aSigned = Random.nextInt(2) == 0
    val bSigned = Random.nextInt(2) == 0
    random(width, aSigned, bSigned)
  }
  def randomUInt(width: Int): MulTestCase = {
    random(width, false, false)
  }
  def randomSInt(width: Int): MulTestCase = {
    random(width, true, true)
  }
}

/** (a, b, Q, R) a = b * Q + R
  */
case class DivTestCase(a: BigInt, b: BigInt, q: BigInt, r: BigInt, width: Int, signed: Boolean) {
  val ua = if (signed) ToSIntBits(a, width) else a
  val ub = if (signed) ToSIntBits(b, width) else b
  val uq = if (signed) ToSIntBits(q, width) else q
  val ur = if (signed) ToSIntBits(r, width) else r
}

object DivTestCase {
  def random(width: Int): DivTestCase = {
    if (Random.nextInt(2) == 0)
      randomUInt(width)
    else
      randomSInt(width)
  }
  def randomUInt(width: Int): DivTestCase = {
    val a = RandomBigInt(width)
    val b = RandomBigInt(width)
    if (b == 0) {
      val q = (1 << width) - 1
      val r = a
      DivTestCase(a, b, q, r, width, false)
    } else {
      val q = a / b
      val r = a - b * q
      DivTestCase(a, b, q, r, width, false)
    }
  }
  def randomSInt(width: Int): DivTestCase = {
    val a = RandomBigInt.signed(width)
    val b = RandomBigInt.signed(width)
    if (b == 0) {
      val q = (1 << width) - 1
      val r = a
      DivTestCase(a, b, q, r, width, true)
    } else {
      val q = a / b
      val r = a - b * q
      DivTestCase(a, b, q, r, width, true)
    }
  }
}

object RandomBigInt {

  /** Get random BigInt under 1 << n
    *
    * @param n
    */
  def apply(n: Int): BigInt = {
    require(n >= 0)

    import scala.util.Random
    var s = ""
    for (i <- 0 until n) {
      s = s + Random.nextInt(2).toString
    }
    return BigInt(s, 2)
  }
  def signed(n: Int): BigInt = {
    require(n >= 0)
    apply(n) - (BigInt(1) << (n - 1))
  }
}

object ToSIntBits {
  def apply(value: BigInt, width: Int): BigInt = {
    value & ((BigInt(1) << width) - 1)
  }
}
