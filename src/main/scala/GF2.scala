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

case class GF2(val value: Int) extends Field[GF2] {
  require(value == 0 || value == 1, "Only 0 or 1 are allowed values for GF2!")

  def +(that: GF2) = {
    new GF2(value ^ that.value)
  }
  def -(that: GF2) = this + -that
  def *(that: GF2) = new GF2(value & that.value)
  def /(that: GF2) = this * that.inv

  def unary_+ = this
  def inv = {
    require(value != 0, "The denominator must not be zero!")
    this
  }
  def unary_- = this

  def zero = new GF2(0)
  def one = new GF2(1)

  override def equals(o: Any) = o match {
    case that: GF2 => value == that.value
    case _ => false
  }

  override def toString() = value.toString
}

object GF2 {
  implicit val gf2num = GF2(1)

  implicit def fromLong(l:Long) = new GF2(l.toInt)
  implicit def fromInt(i:Int) = new GF2(i)
  implicit def fromShort(s:Short) = new GF2(s)
}
