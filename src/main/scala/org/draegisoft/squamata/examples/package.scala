package org.draegisoft.squamata
package object examples{

  /**
  * Multiply polynomials a and b modulo an irreducible polynomial modulus
  * Polynomials are represented by integers, thus they represent scalars of GF(2^n) with
  * n &lt; 32.
  * @param gf tells us, which Galois field the operation takes place in, i.e. gf=128 
  *        would be GF(2^8)
  */
  def gmul(a: Int, b: Int, modulus: Int, gf: Int): Int = {
    def gmul_h(a: Int, b: Int, p: Int): Int = {
      if (b==0) p
      else {
        val pnew = if ((b&1) != 0) p^a else p
        val anew = if ((a&gf) != 0) (a<<1)^modulus else a<<1
        val bnew = b >> 1
        gmul_h(anew, bnew, pnew)
      }
    }
    gmul_h(a, b, 0)
  }

  /**
  * @return the degree of the polynomial a as 2^n
  */
  def degree(a: Int): Int = {
    if (a > 0) 32 - java.lang.Integer.numberOfLeadingZeros(a) - 1
    else 0
  }

  /**
  * Calculate the inverse of a binary field element modulo an irreducible polynomial.
  * Uses the extended Euclidean algorithm.
  * @return the multiplicative inverse of a polynomial mod a polynomial p in GF(2^n)
  */
  def inverse(a: Int, p: Int): Int = {
    def inverse_h(u: Int, v: Int, g1: Int, g2: Int): Int = {
      if (u == 1) g1
      else if (v == 1) g2
      else {
        if (u % 2 == 0) {
          val unew = u >> 1
          val g1new = if(g1 % 2 == 0) g1 >> 1 else (g1 ^ p) >> 1
          return inverse_h(unew, v, g1new, g2)
        }
        if (v % 2 == 0) {
          val vnew = v >> 1
          val g2new = if(g2 % 2 == 0) g2 >> 1 else (g2 ^ p) >> 1
          return inverse_h(u, vnew, g1, g2new)
        }
        if (degree(u) > degree(v)) {
          val unew = u ^ v
          val g1new = g1 ^ g2
          return inverse_h(unew, v, g1new, g2)
        } else {
          val vnew = u ^ v
          val g2new = g1 ^ g2
          return inverse_h(u, vnew, g1, g2new)
        }
      }
    }
    inverse_h(a, p, 1, 0)
  }
}
