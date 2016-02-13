package org.draegisoft.math.field

case class GF2(val value: Int) extends Field[GF2] {
  require(value == 0 || value == 1, "Only 0 or 1 are allowed values for GF2!")

  def +(that: GF2) = {
    new GF2((value + that.value) % 2)
  }
  def -(that: GF2) = this + -that
  def *(that: GF2) = new GF2(value * that.value)
  def /(that: GF2) = this * that.inv

  def unary_+ = this
  def inv = {
    require(value != 0, "The denominator must not be zero!")
    this
  }
  def unary_- = this

  def zero = new GF2(0)
  def one = new GF2(1)

  override def toString() = value.toString
}

object GF2 {
  implicit val gf2num = GF2(1)

  implicit def fromLong(l:Long) = new GF2(l.toInt)
  implicit def fromInt(i:Int) = new GF2(i)
  implicit def fromShort(s:Short) = new GF2(s)
}
