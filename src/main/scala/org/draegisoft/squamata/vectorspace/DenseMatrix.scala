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

case class DenseMatrix[A <: Field[A]] (private val rows: scala.collection.immutable.Vector[DenseVector[A]])
                                      (implicit num: Field[A]) 
                                      extends Matrix[A] {
  val dim = rows.length
  require(rows.forall(v => v.dim == dim), "Not a square matrix!")

  def apply(row: Int): Vector[A] = rows(row)
  def apply(row: Int, column: Int): A = rows(row)(column)

  def +(that: Matrix[A]) = {
    require(dim == that.dim, "Matrix dimensions don't match!")
    DenseMatrix((0 until dim).map(i => rows(i) + that(i)).toVector)
  }
  def -(that: Matrix[A]) = this + -that

  def *(that: Matrix[A]) = {
    require(dim == that.dim, "Dimensions of the matrices don't match!")
    val rhs = ~that
    var newRows = scala.collection.immutable.Vector.empty[DenseVector[A]]
    for (row <- 0 until dim) {
      var newRow = scala.collection.immutable.Vector.empty[A]
      for (column <- 0 until dim) {
        newRow = newRow :+ rows(row) * rhs(column)
      }
      newRows = newRows :+ DenseVector(newRow)
    }
    new DenseMatrix(newRows)
  }

  def *(that: Vector[A]) = {
    require(dim == that.dim, "Dimensions of the matrices don't match!")
    var resultVector = scala.collection.immutable.Vector.empty[A]
    for (row <- 0 until dim) {
      val dotProduct = rows(row) * that
      resultVector = resultVector :+ dotProduct
    }
    new DenseVector(resultVector)
  }

  /**
   * The transpose of the matrix
   **/
  def unary_~ = {
    var newRows = scala.collection.immutable.Vector.empty[DenseVector[A]]
    for (column <- 0 until dim) {
      var newRow = scala.collection.immutable.Vector.empty[A]
      for (row <- 0 until dim) {
        newRow = newRow :+ rows(row)(column)
      }
      newRows = newRows :+ DenseVector(newRow)
    }
    new DenseMatrix(newRows)
  }

  def unary_- = new DenseMatrix(rows map (-_))

  def inverse = {
    val unitVectors = (0 until dim).toList.map(i => (DenseVector.unit(i, dim): DenseVector[A]))
    val x: A = num.zero // just providing `num` didn't work, leading to a type mismatch. `num` and `num.zero` have the same type, though
    solve(this, unitVectors)(x) map (list => ~DenseMatrix(list.toVector.map(v => v.asInstanceOf[DenseVector[A]])))
  }

  def scaleBy(scalar: A) = new DenseMatrix(rows map (v => v.scaleBy(scalar)))

  def updated(index: Int, vector: Vector[A]) = vector match {
    case v: DenseVector[A] => DenseMatrix(rows updated (index, v))
    case _ => throw new IllegalArgumentException("Can only place DenseVector[A] inside DenseMatrix[A]")
  }

  override def equals(o: Any) = o match {
    case that: Matrix[A] => dim == that.dim &&
                            (0 until dim).forall(index => this(index) == that(index))
    case _ => false
  }

  override def toString() = rows.mkString("[", ", ", "]")
}

object DenseMatrix {
  def apply[A <: Field[A]](rows: DenseVector[A]*)(implicit num: Field[A]) = new DenseMatrix(rows.toVector)

  def zeros[A <: Field[A]](length: Int)(implicit num: Field[A]) = 
    DenseMatrix(scala.collection.immutable.Vector.fill(length) { DenseVector.zeros(length) })

  def ones[A <: Field[A]](length: Int)(implicit num: Field[A]) = 
    DenseMatrix(scala.collection.immutable.Vector.fill(length) { DenseVector.ones(length) })

  def identity[A <: Field[A]](dim: Int)(implicit num: Field[A]) = 
    DenseMatrix( (0 until dim).map(index => DenseVector.unit(index, dim)).toVector)
}
