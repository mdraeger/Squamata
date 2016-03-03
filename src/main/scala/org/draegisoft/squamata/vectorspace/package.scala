package org.draegisoft.squamata

import org.draegisoft.squamata.field.Field

package object vectorspace{
  /**
  * Solve a linear system simultaneously using the Gauss-Jordan-Algorithm
  * @return None, if there is no solution and Some(_), otherwise
  */
  def solve[A <: Field[A]](matrix: Matrix[A], vectorList: List[Vector[A]])(implicit num: A): Option[List[Vector[A]]] = {
    // record which rows had to be switched
    var m = matrix
    var vlist = vectorList
    // now start the algorithm, first go forward
    for (i <- 0 until m.dim) {
      // if we got the zero vector, return with no success
      if((0 until m.dim).forall(j => m(i,j) == num.zero)) return None

      // we might have to switch rows, if we encounter zero in the ith column. 
      if (m(i,i) == num.zero) {
        switchRowsIfNecessary(m, vlist, i) match {
          case Some((mat, list)) => m = mat; vlist = list
          case _ => return None
        }
      }

      val scaleFactor = num.one / m(i,i)
      m = m.updated(i, m(i).scaleBy(scaleFactor))
      vlist = vlist map (v => v.updated(i, v(i)*scaleFactor))

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

  private def switchRowsIfNecessary[A <: Field[A]]
                                   (matrix: Matrix[A], vectorList: List[Vector[A]], row: Int)
                                   (implicit num: A): Option[(Matrix[A], List[Vector[A]])] = {
    var m = matrix; var vlist = vectorList
    while (m(row,row) == num.zero) {
      var j = row+1
      if (j == m.dim) return None // The linear systen is underdetermined
      else if (m(j,row) != num.zero){
        m = switch(m, row, j)
        vlist = vlist map (v => switch(v, row, j))
      }
      j += 1
    }
    Some((m, vlist))
  }

  private def subtractScaledRow[A <: Field[A]](m: Matrix[A], vlist: List[Vector[A]], factor: A, row: Int, column: Int) = {
    val newMatrix = m.updated(column, m(column) - m(row).scaleBy(factor))
    val newList = vlist map (v => v.updated(column, v(column) - factor*v(row)))
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
