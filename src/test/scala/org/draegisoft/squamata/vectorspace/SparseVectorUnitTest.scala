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

import org.draegisoft.squamata.field._

import org.draegisoft.squamata.UnitTest

import org.scalatest.{FlatSpec, Matchers}

class SparseVectorUnitTest extends UnitTest("SparseVector") {
  val compVals = List(Complex.i, Complex(1.0), Complex(1.0,1.0), Complex(5.0, -7.0))
  val realVals = List(Real(1.0), Real(0.0), Real(math.Pi), Real(-math.E))
  val gf2Val = List(GF2(1), GF2(0))
  val ratVals = List(Rational(1), Rational(0), Rational(1,7), Rational(15,3))
  val compSparseVector10: List[SparseVector[Complex]] = 
                    List(SparseVector(10, (0, Complex.i), (3, Complex(1,0)), (8, Complex(1,1))), 
                         SparseVector(10, (0, Complex(4)), (2, Complex(0,-4)), (8, Complex(4,4))),
                         SparseVector(10, (1, Complex(-4)), (5, Complex(0,-4)), (8, Complex(4,4))),
                         SparseVector(10, (2, Complex(-math.sqrt(2))), (5, Complex(0,math.sqrt(2))), (8, Complex(4,4))),
                         SparseVector(10, (3, Complex(math.Pi)), (6, Complex(0,-math.E)), (9, Complex(8,-4))))

  it should "cut out a single value when multiplied by a unit vector" in {
    for (index <- (0 until 10); v <- compSparseVector10) {
      val unitVector: SparseVector[Complex] = SparseVector.unit(index, 10)
      (v * unitVector) shouldEqual v(index)
    }
  }

  it should "add vectors correctly" in {
    for (u <- compSparseVector10; v <- compSparseVector10) {
      val sum = u + v
      for (i <- 0 until 10)
        sum(i) shouldEqual (u(i) + v(i))
    }
  }

  it should "(dot) multiply vectors correctly" in {
    for (u <- compSparseVector10; v <- compSparseVector10) {
      val dotProduct = u * v
      (0 until 10).map(i => u(i) * v(i)).foldLeft(Complex(0,0))(_ + _) shouldEqual dotProduct
    }
  }

  it should "create zero vectors correctly" in {
    for (length <- 0 until 500) {
      val zeroVector: SparseVector[Real] = SparseVector.zeros(length)
      for (index <- 0 until length)
        zeroVector(index) shouldEqual Real(0.0)
    }
  }

  it should "create one vectors correctly" in {
    for (length <- 0 until 50) {
      val oneVector: SparseVector[GF2] = SparseVector.ones(length)
      for (index <- 0 until length)
        oneVector(index) shouldEqual GF2(1)
    }
  }

  it should "create unit vectors correctly" in {
    for (length <- 0 until 50; unitIndex <- 0 until length) {
      val unitVector: SparseVector[Rational] = SparseVector.unit(unitIndex, length)
      for (index <- 0 until length)
        if (unitIndex == index)
          unitVector(index) shouldEqual Rational(1)
        else 
          unitVector(index) shouldEqual Rational(0)
    }
  }

  it should "remain unchanged when multiplied by an identity matrix" in {
    val identity = SparseMatrix.identity(10): SparseMatrix[Complex]
    for (v <- compSparseVector10)
      v * identity shouldEqual v
  }

  it should "allow to get updated" in {
    val id0 = SparseVector.unit(0, 5): SparseVector[Real]
    val id1 = SparseVector.unit(1, 5): SparseVector[Real]
    val id2 = SparseVector.unit(2, 5): SparseVector[Real]
    val id3 = SparseVector.unit(3, 5): SparseVector[Real]
    val id4 = SparseVector.unit(4, 5): SparseVector[Real]
    id0 + id1 shouldEqual id0.updated(1, Real(1))
  }
}
