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

case class DenseVector[A <: Field[A]] (private val values: scala.collection.immutable.Vector[A])
                                      (implicit num: Field[A]) 
                                      extends Vector[A] {
  val dim = values.length

  def apply(index: Int): A = values(index)

  def +(that: Vector[A]): DenseVector[A] = {
    require(dim == that.dim, "Vectors must be of same dimension!")
    DenseVector((0 until dim).map(i => values(i) + that(i)).toVector)
  }
  def -(that: Vector[A]) = this + -that

  def *(that: Vector[A]): A = {
    require(dim == that.dim, "Vectors must be of same dimension!")
    val zero = num.zero
    (0 until dim).map(i => values(i) * that(i)).foldLeft(zero)(_ + _)
  }

  def *(that: Matrix[A]): Vector[A] = {
    require(dim == that.dim, "Dimensions of the matrices don't match!")
    val rhs = ~that
    var resultDenseVector = scala.collection.immutable.Vector.empty[A]
    for (column <- 0 until dim) {
      val dotProduct = this * rhs(column)
      resultDenseVector = resultDenseVector :+ dotProduct
    }
    DenseVector(resultDenseVector)
  }

  def unary_- = DenseVector(values map (-_))

  def scaleBy(scalar: A) = DenseVector(values map (_ * scalar))

  override def equals(o: Any) = o match {
    case that: Vector[A] => dim == that.dim &&
                            (0 until dim).forall(index => this(index) == that(index))
    case _ => false
  }

  override def toString() = values.mkString("[", ", ", "]")
}

object DenseVector {
  def apply[A <: Field[A]](values:A*)(implicit num: Field[A]) = new DenseVector(values.toVector)

  def zeros[A <: Field[A]](length: Int)(implicit num: Field[A]): DenseVector[A] =
    DenseVector(scala.collection.immutable.Vector.fill(length) { num.zero })

  def ones[A <: Field[A]](length: Int)(implicit num: Field[A]): DenseVector[A] =
    DenseVector(scala.collection.immutable.Vector.fill(length) { num.one })

  def unit[A <: Field[A]](index:Int, length: Int)(implicit num: Field[A]): DenseVector[A] =
    DenseVector(scala.collection.immutable.Vector.fill(length) { num.zero } .updated(index, num.one))
}
