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

case class SparseMatrix[A <: Field[A]] (val dim: Int, private val rows: Map[Int, SparseVector[A]])
                                       (implicit num: Field[A]) 
                                       extends Matrix[A] {
  require(rows.forall{ case (k, v) => v.dim == dim}, "Not a square matrix!")

  private val sparseZeroVector = SparseVector.zeros(dim): SparseVector[A]

  def apply(row: Int): SparseVector[A] = rows.getOrElse(row, sparseZeroVector)
  def apply(row: Int, column: Int): A = this(row)(column)

  def +(that: Matrix[A]): SparseMatrix[A] =  {
    require(dim == that.dim, "Dimensions of the matrices don't match!")
    SparseMatrix(dim, (0 until dim).map(i => (i, (this(i) + that(i)).asInstanceOf[SparseVector[A]]))
                                   .toMap
                                   .filter{ case (i, v) => v != sparseZeroVector})
  }
  def -(that: Matrix[A]) = this + -that
  def *(that: Matrix[A]): SparseMatrix[A] = {
    val rhs = ~that
    val newRows = (0 until dim).map(i => i -> this * rhs(i))
                               .filter{ case (i, vec) => vec != sparseZeroVector }
                               .toMap
    SparseMatrix(dim, newRows)
  }
  /**
  * @param that: Vector[A] is assumed to be a column vector
  */
  def *(that: Vector[A]): SparseVector[A] = 
    SparseVector(dim, rows.mapValues(v => v * that).filter{ case (i,v) => v != num.zero })

  /**
   * The transpose of the matrix
   **/
  def unary_~ = {
    val columns = (0 until dim).map(i => i -> {
        val newColumn = (0 until dim).map(j => j -> this(j, i))
                                     .filter { case (index, value) => value != num.zero } 
                                     .toMap
        SparseVector(dim, newColumn)
      }).filter { case (index, value) => value != sparseZeroVector }.toMap
    SparseMatrix(dim, columns)
  }
  def unary_- = this.scaleBy(-num.one)

  def scaleBy(scalar: A) = SparseMatrix(dim, rows.mapValues (v => v.scaleBy(scalar)))

  override def toString() = rows.mkString("[", ", ", "]")
}

object SparseMatrix {
  def apply[A <: Field[A]](dim: Int, rows: (Int, SparseVector[A])*)(implicit num: Field[A]) = 
    new SparseMatrix(dim, rows.toMap)

  def zeros[A <: Field[A]](length: Int)(implicit num: Field[A]) = 
    SparseMatrix(length, Map.empty[Int, SparseVector[A]] )

  def ones[A <: Field[A]](length: Int)(implicit num: Field[A]) = 
    SparseMatrix(length, (0 until length).map(i => (i, SparseVector.ones(length))).toMap)

  def identity[A <: Field[A]](dim: Int)(implicit num: Field[A]) = 
    SparseMatrix(dim, (0 until dim).map(i => (i, SparseVector.unit(i, dim))).toMap)
}
