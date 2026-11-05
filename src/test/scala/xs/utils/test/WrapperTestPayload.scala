package xs.utils.test

import chisel3._

/** Unequal-width fields expose field-order, sign-bit and Vec packing mistakes. */
class WrapperTestPacket(width: Int) extends Bundle {
  require(width >= 5, "packet needs room for unsigned, signed and flag fields")
  private val tagWidth = math.min(5, width - 4)
  val tag = UInt(tagWidth.W)
  val delta = SInt((width - tagWidth - 3).W)
  val flags = Vec(3, Bool())
}

/** Retain the one-bit SV boundary case without using a scalar wrapper payload. */
class WrapperTestBit extends Bundle {
  val flag = Bool()
}

object WrapperTestPayload {
  def apply(width: Int, shape: String = "bundle"): Data = shape match {
    case "bundle" => if (width == 1) new WrapperTestBit else new WrapperTestPacket(width)
    case "vecBundle" =>
      require(width % 3 == 0, "Vec[Bundle] payload width must divide into three packets")
      Vec(3, new WrapperTestPacket(width / 3))
    case other => throw new IllegalArgumentException(s"unknown payload shape: $other")
  }

  /** Software-only layout: flags are low, then signed delta, then tag;
    * Vec element zero is low. Never use DUT asUInt/asTypeOf to form expectations.
    */
  def fields(data: Data, bits: BigInt, path: String = "payload"): Seq[(Bits, BigInt, String)] = {
    def mask(width: Int): BigInt = (BigInt(1) << width) - 1
    data match {
      case p: WrapperTestPacket =>
        val width = p.delta.getWidth
        val rawDelta = (bits >> 3) & mask(width)
        val delta = if (rawDelta.testBit(width - 1)) rawDelta - (BigInt(1) << width) else rawDelta
        Seq((p.tag, (bits >> (width + 3)) & mask(p.tag.getWidth), s"$path.tag"),
          (p.delta, delta, s"$path.delta")) ++
          p.flags.zipWithIndex.map { case (flag, i) => (flag, (bits >> i) & 1, s"$path.flags[$i]") }
      case p: WrapperTestBit => Seq((p.flag, bits & 1, s"$path.flag"))
      case v: Vec[_] =>
        v.zipWithIndex.flatMap { case (element, i) => fields(element, bits >> (i * element.getWidth), s"$path[$i]") }.toSeq
      case other => throw new IllegalArgumentException(s"expected Bundle or Vec[Bundle], got $other")
    }
  }
}
