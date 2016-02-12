package org.draegisoft.math.field

case class Real(val re: Double) extends Field[Real] {

  def +(that: Real) = new Real(re + that.re)
  def -(that: Real) = this + -that
  def *(that: Real) = new Real(re*that.re)
  def /(that: Real) = this * that.inv

  def unary_+ = this
  def inv = {
    require(re != 0, "The denominator must not be zero!")
    new Real(1.0/ re) // multiplicative inverse
  }
  def unary_- = new Real(-re)

  def zero = new Real(0.0)
  def one = new Real(1.0)

  override def toString() = re.toString
}

object Real {
  implicit val realNum = Real(1.0)

  implicit def fromDouble(d:Double) = new Real(d)
  implicit def fromFloat(f:Float) = new Real(f)
  implicit def fromLong(l:Long) = new Real(l)
  implicit def fromInt(i:Int) = new Real(i)
  implicit def fromShort(s:Short) = new Real(s)
}
