package org.draegisoft.math.vectorspace

import org.draegisoft.math.field.Field

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
    new Vector(resultVector)
  }

  def unary_- = new Vector(values map (-_))

  def scaleBy(scalar: A) = new Vector(values map (_ * scalar))

  override def toString() = "[" + values.mkString(", ") + "]"
}

object Vector {
  def apply[A <: Field[A]](values:A*) = new Vector(values.toVector)

  def zeros[A <: Field[A]](length: Int)(implicit num:Field[A]): Vector[A] =
    new Vector(scala.collection.immutable.Vector.fill(length) { num.zero })

  def ones[A <: Field[A]](length: Int)(implicit num:Field[A]): Vector[A] =
    new Vector(scala.collection.immutable.Vector.fill(length) { num.one })
}
