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

class MatrixUnitTest extends UnitTest("Matrix") {

  it should "create zero matrices correctly" in {
    for (dim <-1 until 20) {
      val theMatrix = Matrix.zeros(dim): Matrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        theMatrix(i,j) shouldEqual Real(0)
    }
  }

  it should "create one matrices correctly" in {
    for (dim <-1 until 20) {
      val theMatrix = Matrix.ones(dim): Matrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        theMatrix(i,j) shouldEqual Real(1)
    }
  }

  it should "create identity matrices correctly" in {
    for (dim <-1 until 20) {
      val theMatrix = Matrix.identity(dim): Matrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        if (i == j)
          theMatrix(i,j) shouldEqual Real(1)
        else
          theMatrix(i,j) shouldEqual Real(0)
    }
  }

  it should "remain unchanged when multiplied by an identity matrix" in {
  }
}
