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

package org.draegisoft.squamata.vectorspace

import org.draegisoft.squamata.field.Field

case class Vector[A <: Field[A]] (private val values: scala.collection.immutable.Vector[A]) {
  val dim = values.length

  def apply(index: Int): A = values(index)

  def +(that: Vector[A]): Vector[A] = {
    require(dim == that.dim, "Vectors must be of same dimension!")
    val sums = (values, that.values).zipped.map(_+_)
    new Vector(sums)
}
  def -(that: Vector[A]) = this + -that

  def *(that: Vector[A])(implicit num: Field[A]): A = {
    val zero = num.zero
    (values, that.values).zipped.map(_*_).foldLeft(zero)(_ + _)
  }

  def *(that: Matrix[A])(implicit num: Field[A]): Vector[A] = {
    val rhs = ~that
    var resultVector = scala.collection.immutable.Vector.empty[A]
    for (column <- 0 until dim) {
      val dotProduct = this * rhs(column)
      resultVector = resultVector :+ dotProduct
    }
    Vector(resultVector)
  }

  def unary_- = Vector(values map (-_))

  def scaleBy(scalar: A) = Vector(values map (_ * scalar))

  override def toString() = "[" + values.mkString(", ") + "]"
}

object Vector {
  def apply[A <: Field[A]](values:A*) = new Vector(values.toVector)

  def zeros[A <: Field[A]](length: Int)(implicit num:Field[A]): Vector[A] =
    Vector(scala.collection.immutable.Vector.fill(length) { num.zero })

  def ones[A <: Field[A]](length: Int)(implicit num:Field[A]): Vector[A] =
    Vector(scala.collection.immutable.Vector.fill(length) { num.one })

  def unit[A <: Field[A]](index:Int, length: Int)(implicit num:Field[A]): Vector[A] =
    Vector(scala.collection.immutable.Vector.fill(length) { num.zero } .updated(index, num.one))
}
