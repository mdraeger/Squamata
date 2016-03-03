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

class DenseMatrixUnitTest extends UnitTest("DenseMatrix") {

  it should "create zero matrices correctly" in {
    for (dim <-1 until 20) {
      val theDenseMatrix = DenseMatrix.zeros(dim): DenseMatrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        theDenseMatrix(i,j) shouldEqual Real(0)
    }
  }

  it should "create one matrices correctly" in {
    for (dim <-1 until 20) {
      val theDenseMatrix = DenseMatrix.ones(dim): DenseMatrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        theDenseMatrix(i,j) shouldEqual Real(1)
    }
  }

  it should "create identity matrices correctly" in {
    for (dim <-1 until 20) {
      val theDenseMatrix = DenseMatrix.identity(dim): DenseMatrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        if (i == j)
          theDenseMatrix(i,j) shouldEqual Real(1)
        else
          theDenseMatrix(i,j) shouldEqual Real(0)
    }
  }

  it should "remain unchanged when multiplied by an identity matrix" in {
  }
}
