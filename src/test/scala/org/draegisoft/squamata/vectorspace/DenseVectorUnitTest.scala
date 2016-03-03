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

class DenseVectorUnitTest extends UnitTest("DenseVector") {
  val compVals = List(Complex.i, Complex(1.0), Complex(1.0,1.0), Complex(5.0, -7.0))
  val realVals = List(Real(1.0), Real(0.0), Real(math.Pi), Real(-math.E))
  val gf2Val = List(GF2(1), GF2(0))
  val ratVals = List(Rational(1), Rational(0), Rational(1,7), Rational(15,3))
  val compDenseVector3 = List(DenseVector(Complex.i, Complex(1,0), Complex(1,1)), 
                         DenseVector(Complex(4), Complex(0,-4), Complex(4,4)),
                         DenseVector(Complex(-4), Complex(0,-4), Complex(4,4)),
                         DenseVector(Complex(-math.sqrt(2)), Complex(0,math.sqrt(2)), Complex(4,4)),
                         DenseVector(Complex(math.Pi), Complex(0,-math.E), Complex(8,-4)))

  it should "cut out a single value when multiplied by a unit vector" in {
    for (v <- compDenseVector3; i <- 0 until 3) {
      val u: DenseVector[Complex] = DenseVector.unit(i,3)
      (u * v) shouldEqual v(i)
    }
  }

  it should "add vectors correctly" in {
    for (u <- compDenseVector3; v <- compDenseVector3) {
      val uVals = (0 until 3).map(i => u(i)).toVector
      val vVals = (0 until 3).map(i => v(i)).toVector
      val resVal = (uVals, vVals).zipped.map(_ + _)
      (u + v) shouldEqual DenseVector(resVal)
    }
  }

  it should "(dot) multiply vectors correctly" in {
    for (u <- compDenseVector3; v <- compDenseVector3) {
      val uVals = (0 until 3).map(i => u(i)).toVector
      val vVals = (0 until 3).map(i => v(i)).toVector
      val zero = Complex(0,0)
      val res = (uVals, vVals).zipped.map(_ * _).foldLeft(zero)(_ + _)
      (u * v) shouldEqual res
    }
  }

  it should "create zero vectors correctly" in {
    (DenseVector.zeros(5): DenseVector[GF2]) shouldEqual DenseVector(GF2(0),GF2(0),GF2(0),GF2(0),GF2(0))
    (DenseVector.zeros(5): DenseVector[Rational]) shouldEqual DenseVector(Rational(0),Rational(0),Rational(0),Rational(0),Rational(0))
    (DenseVector.zeros(5): DenseVector[Complex]) shouldEqual DenseVector(Complex(0),Complex(0),Complex(0),Complex(0),Complex(0))
    (DenseVector.zeros(5): DenseVector[Real]) shouldEqual DenseVector(Real(0),Real(0),Real(0),Real(0),Real(0))
  }

  it should "create one vectors correctly" in {
    (DenseVector.ones(5): DenseVector[GF2]) shouldEqual DenseVector(GF2(1),GF2(1),GF2(1),GF2(1),GF2(1))
    (DenseVector.ones(5): DenseVector[Rational]) shouldEqual DenseVector(Rational(1),Rational(1),Rational(1),Rational(1),Rational(1))
    (DenseVector.ones(5): DenseVector[Complex]) shouldEqual DenseVector(Complex(1),Complex(1),Complex(1),Complex(1),Complex(1))
    (DenseVector.ones(5): DenseVector[Real]) shouldEqual DenseVector(Real(1),Real(1),Real(1),Real(1),Real(1))
  }

  it should "create unit vectors correctly" in {
    (DenseVector.unit(2,5): DenseVector[GF2]) shouldEqual DenseVector(GF2(0),GF2(0),GF2(1),GF2(0),GF2(0))
    (DenseVector.unit(1,5): DenseVector[Rational]) shouldEqual DenseVector(Rational(0),Rational(1),Rational(0),Rational(0),Rational(0))
    (DenseVector.unit(0,5): DenseVector[Complex]) shouldEqual DenseVector(Complex(1),Complex(0),Complex(0),Complex(0),Complex(0))
    (DenseVector.unit(4,5): DenseVector[Real]) shouldEqual DenseVector(Real(0),Real(0),Real(0),Real(0),Real(1))
  }

  it should "remain unchanged when multiplied by an identity matrix" in {
    for (v <- compDenseVector3) 
      v * (DenseMatrix.identity(3): Matrix[Complex]) shouldEqual v
  }

  it should "allow to get updated" in {
    val id0 = DenseVector.unit(0, 5): DenseVector[Real]
    val id1 = DenseVector.unit(1, 5): DenseVector[Real]
    val id2 = DenseVector.unit(2, 5): DenseVector[Real]
    val id3 = DenseVector.unit(3, 5): DenseVector[Real]
    val id4 = DenseVector.unit(4, 5): DenseVector[Real]
    id0 + id1 shouldEqual id0.updated(1, Real(1))
  }
}
