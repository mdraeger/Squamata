/*
 Copyright 2016 Marco Draeger

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package org.draegisoft.squamata.field

case class Rational(val a: Int, val b: Int) extends Field[Rational] {
  require(b != 0, "The denominator must not be zero!")
  
  val (numerator, denominator) = {
    val g = gcd(a.abs,b.abs)
    if (b < 0) (-a / g, b.abs /g)
    else (a / g, b.abs / g)
  }

  private def gcd(m: Int, n: Int): Int =
    if (n == 0) m else gcd(n, m%n)

  def +(that: Rational) = {
    val n = numerator * that.denominator + denominator * that.numerator
    val d = denominator * that.denominator
    new Rational(n, d)
  }
  def -(that: Rational) = this + -that
  def *(that: Rational) = new Rational(numerator * that.numerator, denominator * that.denominator)
  def /(that: Rational) = this * that.inv

  def unary_+ = this
  def inv = {
    require(numerator != 0, "The denominator must not be zero!")
    new Rational(denominator, numerator) // multiplicative inverse
  }
  def unary_- = new Rational(-numerator, denominator)

  def zero = new Rational(0, 1)
  def one = new Rational(1, 1)

  override def equals(that: Any) = that match {
    case r: Rational => numerator == r.numerator && denominator == r.denominator
    case _ => false
  }

  override def toString() = 
    if (numerator == 0) "0" 
    else denominator match {
        case 1 => numerator.toString
        case _ => numerator + "/" + denominator
      }
}

object Rational {
  def apply(n: Int) = new Rational(n, 1)

  implicit val rationalNum = Rational(1,1)

  implicit def fromLong(l:Long) = new Rational(l.toInt, 1)
  implicit def fromInt(i:Int) = new Rational(i, 1)
  implicit def fromShort(s:Short) = new Rational(s, 1)
}
