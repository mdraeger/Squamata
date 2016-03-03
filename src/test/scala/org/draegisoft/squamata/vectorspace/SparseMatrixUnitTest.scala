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

class SparseMatrixUnitTest extends UnitTest("SparseMatrix") {

  it should "create zero matrices correctly" in {
    for (dim <-1 until 20) {
      val theSparseMatrix = SparseMatrix.zeros(dim): SparseMatrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        theSparseMatrix(i,j) shouldEqual Real(0)
    }
  }

  it should "create one matrices correctly" in {
    for (dim <-1 until 20) {
      val theSparseMatrix = SparseMatrix.ones(dim): SparseMatrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        theSparseMatrix(i,j) shouldEqual Real(1)
    }
  }

  it should "create identity matrices correctly" in {
    for (dim <-1 until 20) {
      val theSparseMatrix = SparseMatrix.identity(dim): SparseMatrix[Real]
      for (i <- 0 until dim; j <- 0 until dim)
        if (i == j)
          theSparseMatrix(i,j) shouldEqual Real(1)
        else
          theSparseMatrix(i,j) shouldEqual Real(0)
    }
  }

  it should "remain unchanged when multiplied by an identity matrix" in {
    val ones = SparseMatrix.ones(50): SparseMatrix[Real]
    ones * (SparseMatrix.identity(50): SparseMatrix[Real]) shouldEqual ones
    val zeros = SparseMatrix.zeros(500): SparseMatrix[GF2]
    zeros * (SparseMatrix.identity(500): SparseMatrix[GF2]) shouldEqual zeros
    val identity = SparseMatrix.identity(150): SparseMatrix[Rational]
    identity * (SparseMatrix.identity(150): SparseMatrix[Rational]) shouldEqual identity
  }

  it should "return twice its value when added to itself" in {
    val ones = SparseMatrix.ones(50): SparseMatrix[Real]
    ones + ones shouldEqual ones.scaleBy(Real(2.0))
  }

  it should "return the zero matrix when subtracted from itself" in {
    val ones = SparseMatrix.ones(50): SparseMatrix[Real]
    val zeros = SparseMatrix.zeros(50): SparseMatrix[Real]
    ones - ones shouldEqual zeros
  }

  it should "transpose symmetric matrices by leaving them unchanged" in {
    val ones = SparseMatrix.ones(50): SparseMatrix[Real]
    val zeros = SparseMatrix.zeros(500): SparseMatrix[GF2]
    val identity = SparseMatrix.identity(150): SparseMatrix[Rational]
    ~ones shouldEqual ones
    ~zeros shouldEqual zeros
    ~identity shouldEqual identity
  }
}
