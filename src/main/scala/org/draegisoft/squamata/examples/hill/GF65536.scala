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

package org.draegisoft.squamata.examples.hill

import org.draegisoft.squamata.field.Field

case class GF65536(val value: Int) extends Field[GF65536] {
  require(value < 65536, "the value cannot exceed the value 65536")
  private val v = if (value < 0) -value else value
  private val p = 0x1100b // x^16 + x^12 + x^3 + x + 1 : the irreducible polynomial
  private val field = 0x10000 / 2 // the basis of this particular field 2^(16 - 1)

  def intValue: Int = v

  def +(that: GF65536) = {
    new GF65536(v ^ that.v)
  }
  def -(that: GF65536) = this + -that
  def *(that: GF65536) = new GF65536(gmul(v, that.v, p, field))
  def /(that: GF65536) = this * that.inv

  def unary_+ = this
  def inv = {
    require(v != 0, "The denominator must not be zero!")
    GF65536(inverse(v, p))
  }
  def unary_- = this

  def zero = new GF65536(0)
  def one = new GF65536(1)

  override def equals(o: Any) = o match {
    case that: GF65536 => v == that.v
    case _ => false
  }

  override def toString() = v.toString
}

object GF65536 {
  implicit val gf2num = GF65536(1)

  implicit def fromLong(l:Long) = new GF65536(l.toInt)
  implicit def fromInt(i:Int) = new GF65536(i)
  implicit def fromShort(s:Short) = new GF65536(s)
}
