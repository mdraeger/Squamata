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

package org.draegisoft.squamata.field

import org.draegisoft.squamata.UnitTest

import org.scalatest.{FlatSpec, Matchers}

import Complex.i
    
class ComplexUnitTest extends UnitTest("Complex") {
  private val root2 = math.sqrt(2.0)

  it should "define the right constants" in {
    i shouldEqual (new Complex(0,1))
    (new Complex(0,0)).zero shouldEqual (new Complex(0,0))
    (new Complex(0,0)).one  shouldEqual (new Complex(1,0))
  }

  it should "deny division by zero" in {
    an [IllegalArgumentException] should be thrownBy {
      i / (new Complex(0,0))
    }
  }

  it should "add numbers correctly" in {
    (Complex(7,5) + Complex(8, -3)) shouldEqual Complex(15, 2)
    (Complex(2,3) + Complex(-8, -6)) shouldEqual Complex(-6, -3)
    (Complex(3,8) + Complex(10, -5)) shouldEqual Complex(13,3)
    (Complex(3,8*root2) + Complex(10, -5*root2)) shouldEqual Complex(13,3*root2)
  }

  it should "subtract numbers correctly" in {
    (Complex(5,8) - Complex(2, 2)) shouldEqual Complex(3, 6)
    (Complex(4,10) - Complex(-12, 20)) shouldEqual Complex(16, -10)
    Complex(9, 6) - Complex(8,-10) shouldEqual Complex(1,16)
  }

  it should "multiply numbers correctly" in {
    Complex(2,3) * Complex(4,5) shouldEqual Complex(-7, 22)
  }

  it should "divide numbers correctly" in {
    Complex(4,2) / Complex(3,-1) shouldEqual Complex(1,1)
  }

  it should "negate numbers correctly" in {
    -i shouldEqual Complex(0, -1)
    -Complex(1,1) shouldEqual Complex(-1, -1)
  }

  it should "build the conjugate correctly" in {
    ~i shouldEqual Complex(0, -1)
    ~Complex(1,1) shouldEqual Complex(1, -1)
  }

  it should "add and multiply with integers" in {
    (2 + 5*i) shouldEqual Complex(2, 5)
  }
}
