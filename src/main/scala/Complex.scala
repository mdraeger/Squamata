package org.draegisoft.math.field

import math.{pow, sqrt}

case class Complex(val re: Double, val im: Double) extends Field[Complex] {
  private val modulus = sqrt(pow(re, 2) + pow(im, 2))
  private lazy val asString = re + (if (im < 0) "-" + -im else "+" + im) + "*i"

  def this(re: Double) = this(re, 0)

  def +(that: Complex) = new Complex(re + that.re, im + that.im)
  def -(that: Complex) = this + -that
  def *(that: Complex) = new Complex(re*that.re - im*that.im, re*that.im + im*that.re)
  def /(that: Complex) = this * that.inv
  def unary_+ = this
  def unary_~ = new Complex(re, -im) // conjugate
  def unary_- = new Complex(-re, -im)
  def unary_! = modulus
  def inv = {
    require(re != 0 || im != 0, "The denominator must not be zero!")
    val d = pow(re, 2) + pow(im, 2)
    new Complex(re/d, -im/d)
  }

  def zero = new Complex(0.0)
  def one = new Complex(1.0)

  override def toString() = 
    this match {
      case Complex.i => "i"
      case Complex(re, 0) => re.toString
      case Complex(0, im) => im.toString + "*i"
      case _ => asString
    }
}

object Complex {
  val i = new Complex(0, 1)
  implicit val complexNum = i

  def apply(re:Double) = new Complex(re)

  implicit def fromDouble(d:Double) = new Complex(d)
  implicit def fromFloat(f:Float) = new Complex(f)
  implicit def fromLong(l:Long) = new Complex(l)
  implicit def fromInt(i:Int) = new Complex(i)
  implicit def fromShort(s:Short) = new Complex(s)
}
