package org.draegisoft.math.vectorspace

import org.draegisoft.math.field.Field

case class Matrix[A <: Field[A]] (private val rows: scala.collection.immutable.Vector[Vector[A]]) {
  val dim = rows.length
  require(rows.forall(v => v.dim == dim), "Not a square matrix!")

  def apply(row: Int): Vector[A] = rows(row)
  def apply(row: Int, column: Int): A = rows(row)(column)

  def +(that: Matrix[A]) = {
    new Matrix((rows, that.rows).zipped.map(_ + _))
  }
  def -(that: Matrix[A]) = this + -that

  def *(that: Matrix[A])(implicit num: Field[A]) = {
    val rhs = ~that
    var newRows = scala.collection.immutable.Vector.empty[Vector[A]]
    for (row <- 0 until dim) {
      var newRow = scala.collection.immutable.Vector.empty[A]
      for (column <- 0 until dim) {
        newRow = newRow :+ rows(row) * rhs.rows(column)
      }
      newRows = newRows :+ Vector(newRow)
    }
    new Matrix(newRows)
  }

  def *(that: Vector[A])(implicit num: Field[A]) = {
    var resultVector = scala.collection.immutable.Vector.empty[A]
    for (row <- 0 until dim) {
      val dotProduct = rows(row) * that
      resultVector = resultVector :+ dotProduct
    }
    new Vector(resultVector)
  }

  /**
   * The transpose of the matrix
   **/
  def unary_~ = {
    var newRows = scala.collection.immutable.Vector.empty[Vector[A]]
    for (column <- 0 until dim) {
      var newRow = scala.collection.immutable.Vector.empty[A]
      for (row <- 0 until dim) {
        newRow = newRow :+ rows(row)(column)
      }
      newRows = newRows :+ Vector(newRow)
    }
    new Matrix(newRows)
  }

  def unary_- = new Matrix(rows map (-_))

  def scaleBy(scalar: A) = new Matrix(rows map (v => v.scaleBy(scalar)))

  override def toString() = "[" + rows.mkString(", ") + "]"
}

object Matrix {
  def apply[A <: Field[A]](rows: Vector[A]*) = new Matrix(rows.toVector)

  def zeros[A <: Field[A]](length: Int)(implicit num:Field[A]): Matrix[A] = ???

  def ones[A <: Field[A]](length: Int)(implicit num:Field[A]): Matrix[A] = ???

  def identity[A <: Field[A]](dim: Int)(implicit num:Field[A]): Matrix[A] = ???
}
