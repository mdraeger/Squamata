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

class PackageUnitTest  extends UnitTest("PackageObject") {
  val x = DenseVector(collection.immutable.Vector(Rational(0), Rational(1), Rational(3)))
  val xSwitched = DenseVector(collection.immutable.Vector(Rational(3), Rational(1), Rational(0)))
  val m = DenseMatrix(collection.immutable.Vector(
    DenseVector(collection.immutable.Vector(Rational(1), Rational(1), Rational(1))),
    DenseVector(collection.immutable.Vector(Rational(4), Rational(2), Rational(1))),
    DenseVector(collection.immutable.Vector(Rational(9), Rational(3), Rational(1)))
    ))
  val mSwitched = DenseMatrix(collection.immutable.Vector(
    DenseVector(collection.immutable.Vector(Rational(1), Rational(1), Rational(1))),
    DenseVector(collection.immutable.Vector(Rational(9), Rational(3), Rational(1))),
    DenseVector(collection.immutable.Vector(Rational(4), Rational(2), Rational(1)))
    ))


  it should "switch the values in a vector" in {
    switch(x, 0, 2) shouldEqual xSwitched
  }

  it should "switch the rows in a matrix" in {
    switch(m, 1, 2) shouldEqual mSwitched
  }

  it should "solve a linear system correctly" in {
    val solution = DenseVector(
      collection.immutable.Vector(Rational(1,2), Rational(-1,2), Rational(0))
    )
    solve(m, List(x)) shouldEqual Some(List(solution))
  }

  it should "solve a linear system correctly when rows are switched" in {
    val solution = DenseVector(
      collection.immutable.Vector(Rational(1,2), Rational(-1,2), Rational(0))
    )
    val calcSol = solve(mSwitched, List(switch(x, 1, 2))) 
    calcSol match {
      case Some(List(v)) => mSwitched * v shouldEqual(switch (x, 1, 2))
      case _ => 
    }
    calcSol shouldEqual Some(List(solution))
  }

  it should "solve a linear system when switching rows is necessary" in {
    val solution = DenseVector(
      collection.immutable.Vector(Rational(-14), Rational(15), Rational(-5), Rational(-13))
    )
    val theMatrix = DenseMatrix(collection.immutable.Vector(
    DenseVector(collection.immutable.Vector(Rational(0), Rational(10), Rational(0), Rational(12))),
    DenseVector(collection.immutable.Vector(Rational(4), Rational(-5), Rational(-2), Rational(-8))),
    DenseVector(collection.immutable.Vector(Rational(0), Rational(-5), Rational(0), Rational(-4))),
    DenseVector(collection.immutable.Vector(Rational(4), Rational(-10), Rational(0), Rational(-16)))
    ))
    val b = DenseVector(collection.immutable.Vector(Rational(-6), Rational(-17), Rational(-23), Rational(2)))

    val calcSol = solve(theMatrix, List(b))
    calcSol match {
      case Some(List(v)) => theMatrix * v shouldEqual b
      case _ => 
    }
    calcSol shouldEqual Some(List(solution))
  }
}
