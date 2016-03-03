package org.draegisoft.squamata

import org.draegisoft.squamata.field.Field

package object vectorspace{
  /**
  * Solve a linear system simultaneously using the Gauss-Jordan-Algorithm
  * @return None, if there is no solution and Some(_), otherwise
  */
  def solve[A <: Field[A]](matrix: Matrix[A], vectorList: List[Vector[A]])(implicit num: A): Option[List[Vector[A]]] = {
    // record which rows had to be switched
    var switches = List.empty[(Int,Int)]
    var m = matrix
    var vlist = vectorList
    // now start the algorithm, first go forward
    for (i <- 0 until m.dim) {
      // if we got the zero vector, return with no success
      if((0 until m.dim).forall(j => m(i,j) == num.zero)) return None

      // we might have to switch rows, if we encounter zero in the ith column. Implement that later
      while (m(i,i) == num.zero) {
        var j = i+1
        if (j == m.dim) return None // The linear systen is underdetermined
        else if (m(j,i) != num.zero){
          m = switch(m, i, j)
          vlist = vlist map (v => switch(v, i, j))
        }
        j += 1
      }

      val scaleFactor = num.one / m(i,i)
      scaleRow(m, vlist, scaleFactor, i) match {
        case (mat, list) => m = mat; vlist = list
        case _ =>
      }
      val pivot = m(i,i)
      for (j <- (i+1) until m.dim) {
        val factor = m(j,i)
        subtractScaledRow(m, vlist, factor, i, j) match {
          case (mat, list) => m = mat; vlist = list
          case _ =>
        }
      }
    }

    // then backward
    for (i <- (m.dim-1) to 0 by -1) {
      // if we got the zero vector, return with no success
      if((0 until m.dim).forall(j => m(i,j) == num.zero)) return None

      val scaleFactor = num.one / m(i,i)
      scaleRow(m, vlist, scaleFactor, i) match {
        case (mat, list) => m = mat; vlist = list
        case _ =>
      }
      val pivot = m(i,i)
      for (j <- i-1 to 0 by -1) {
        val factor = m(j,i)
        subtractScaledRow(m, vlist, factor, i, j) match {
          case (mat, list) => m = mat; vlist = list
          case _ =>
        }
      }
    }
    Some(vlist)
  }

  private def subtractScaledRow[A <: Field[A]](m: Matrix[A], vlist: List[Vector[A]], factor: A, row: Int, column: Int) = {
    val newMatrix = m.updated(column, m(column) - m(row).scaleBy(factor))
    val newList = vlist map (v => v.updated(column, v(column) - factor*v(row)))
    (newMatrix, newList)
  }

  private def scaleRow[A <: Field[A]](m: Matrix[A], vlist: List[Vector[A]], factor: A, row: Int) = {
    val newMatrix = m.updated(row, m(row).scaleBy(factor))
    val newList = vlist map (v => v.updated(row, v(row)*factor))
    (newMatrix, newList)
  }

  /**
  * Switch the rows in a matrix, returning a new matrix.
  */
  def switch[A <: Field[A]](m: Matrix[A], from: Int, to: Int): Matrix[A] = {
    val tmp = m(from)
    m.updated(from, m(to)).updated(to, tmp)
  }

  /**
  * Switch the values in a vector, returning a new vector.
  */
  def switch[A <: Field[A]](v: Vector[A], from: Int, to: Int): Vector[A] = {
    val tmp = v(from)
    v.updated(from, v(to)).updated(to, tmp)
  }
}
