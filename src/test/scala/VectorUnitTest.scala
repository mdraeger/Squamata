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

class VectorUnitTest extends UnitTest("Vector") {
  val compVals = List(Complex.i, Complex(1.0), Complex(1.0,1.0), Complex(5.0, -7.0))
  val realVals = List(Real(1.0), Real(0.0), Real(math.Pi), Real(-math.E))
  val gf2Val = List(GF2(1), GF2(0))
  val ratVals = List(Rational(1), Rational(0), Rational(1,7), Rational(15,3))
  val compVector3 = List(Vector(Complex.i, Complex(1,0), Complex(1,1)), 
                         Vector(Complex(4), Complex(0,-4), Complex(4,4)),
                         Vector(Complex(-4), Complex(0,-4), Complex(4,4)),
                         Vector(Complex(-math.sqrt(2)), Complex(0,math.sqrt(2)), Complex(4,4)),
                         Vector(Complex(math.Pi), Complex(0,-math.E), Complex(8,-4)))

  it should "cut out a single value when multiplied by a unit vector" in {
    for (v <- compVector3; i <- 0 until 3) {
      val u: Vector[Complex] = Vector.unit(i,3)
      (u * v) shouldEqual v(i)
    }
  }

  it should "add vectors correctly" in {
    for (u <- compVector3; v <- compVector3) {
      val uVals = (0 until 3).map(i => u(i)).toVector
      val vVals = (0 until 3).map(i => v(i)).toVector
      val resVal = (uVals, vVals).zipped.map(_ + _)
      (u + v) shouldEqual Vector(resVal)
    }
  }

  it should "(dot) multiply vectors correctly" in {
    for (u <- compVector3; v <- compVector3) {
      val uVals = (0 until 3).map(i => u(i)).toVector
      val vVals = (0 until 3).map(i => v(i)).toVector
      val zero = Complex(0,0)
      val res = (uVals, vVals).zipped.map(_ * _).foldLeft(zero)(_ + _)
      (u * v) shouldEqual res
    }
  }

  it should "create zero vectors correctly" in {
    (Vector.zeros(5): Vector[GF2]) shouldEqual Vector(GF2(0),GF2(0),GF2(0),GF2(0),GF2(0))
    (Vector.zeros(5): Vector[Rational]) shouldEqual Vector(Rational(0),Rational(0),Rational(0),Rational(0),Rational(0))
    (Vector.zeros(5): Vector[Complex]) shouldEqual Vector(Complex(0),Complex(0),Complex(0),Complex(0),Complex(0))
    (Vector.zeros(5): Vector[Real]) shouldEqual Vector(Real(0),Real(0),Real(0),Real(0),Real(0))
  }

  it should "create one vectors correctly" in {
    (Vector.ones(5): Vector[GF2]) shouldEqual Vector(GF2(1),GF2(1),GF2(1),GF2(1),GF2(1))
    (Vector.ones(5): Vector[Rational]) shouldEqual Vector(Rational(1),Rational(1),Rational(1),Rational(1),Rational(1))
    (Vector.ones(5): Vector[Complex]) shouldEqual Vector(Complex(1),Complex(1),Complex(1),Complex(1),Complex(1))
    (Vector.ones(5): Vector[Real]) shouldEqual Vector(Real(1),Real(1),Real(1),Real(1),Real(1))
  }

  it should "create unit vectors correctly" in {
    (Vector.unit(2,5): Vector[GF2]) shouldEqual Vector(GF2(0),GF2(0),GF2(1),GF2(0),GF2(0))
    (Vector.unit(1,5): Vector[Rational]) shouldEqual Vector(Rational(0),Rational(1),Rational(0),Rational(0),Rational(0))
    (Vector.unit(0,5): Vector[Complex]) shouldEqual Vector(Complex(1),Complex(0),Complex(0),Complex(0),Complex(0))
    (Vector.unit(4,5): Vector[Real]) shouldEqual Vector(Real(0),Real(0),Real(0),Real(0),Real(1))
  }

  it should "remain unchanged when multiplied by an identity matrix" in {
    for (v <- compVector3) 
      v * (Matrix.identity(3): Matrix[Complex]) shouldEqual v
  }
}
