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

case class SparseVector[A <: Field[A]] (val dim: Int, private val values: Map[Int,A])
                                       (implicit num: Field[A]) 
                                       extends Vector[A] {

  def apply(index: Int): A = values.getOrElse(index, num.zero)

  def +(that: Vector[A]): SparseVector[A] = {
    require(dim == that.dim, "Dimensions don't match!")
    SparseVector(dim, (0 until dim).map(i => (i, this(i) + that(i)))
                                   .toMap
                                   .filter{ case (i, v) => v != num.zero })
  }
  
  def -(that: Vector[A]) = this + -that

  def *(that: Vector[A]): A = {
    require(dim == that.dim, "Dimensions don't match!")
    val products = (values.keySet).map(i => this(i) * that(i))
    products.foldLeft(num.zero)(_ + _)
  }

  def *(that: Matrix[A]): SparseVector[A] = {
    require(dim == that.dim, "Dimensions of the matrices don't match!")
    val rhs = ~that
    SparseVector(dim, (0 until dim).map(column => (column, this * rhs(column)))
                                   .filter{ case (i, v) => v != num.zero }
                                   .toMap)
  }

  def unary_- = this.scaleBy(-num.one)

  def scaleBy(scalar: A) = SparseVector(dim, values.mapValues(value => scalar*value))

  override def equals(o: Any) = o match {
    case that: SparseVector[A] => dim == that.dim &&
                                  (values.keySet ++ that.values.keySet).forall(
                                    key => this(key) == that(key)
                                  )
    case that: Vector[A] => dim == that.dim &&
                            (0 until dim).forall(index => this(index) == that(index))
    case _ => false
  }

  override def toString() = values.mkString("[", ", ", "]")
}

object SparseVector {
  def apply[A <: Field[A]](dim: Int, values:(Int, A)*)(implicit num: Field[A]): SparseVector[A] = 
    SparseVector(dim, values.toMap)

  def zeros[A <: Field[A]](length: Int)(implicit num:Field[A]): SparseVector[A] = 
    SparseVector(length, Map.empty[Int, A])

  def ones[A <: Field[A]](length: Int)(implicit num:Field[A]): SparseVector[A] = 
    SparseVector(length, (0 until length).map(i => (i, num.one)).toMap)

  def unit[A <: Field[A]](index:Int, length: Int)(implicit num:Field[A]): SparseVector[A] = 
    SparseVector(length, (index, num.one))
}
