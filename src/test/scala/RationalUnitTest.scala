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

class RationalUnitTest extends UnitTest("Rational") {
  it should "Instantiate Rationals correctly" in {
    Rational(1,-1) shouldEqual Rational(-1)
    Rational(-1,-1) shouldEqual Rational(1)
    Rational(-5,-5) shouldEqual Rational(1)
    Rational(5,-7) shouldEqual Rational(-5,7)
  }

  it should "define the right constants" in {
    Rational(0).zero shouldEqual Rational(0,1)
    Rational(0).one  shouldEqual Rational(1,1)
  }

  it should "deny division by zero" in {
    an [IllegalArgumentException] should be thrownBy {
      Rational(1,1) / Rational(0,1)
    }
    an [IllegalArgumentException] should be thrownBy {
      Rational(1,0)
    }
  }

  it should "add numbers correctly" in {
    Rational(1,2) + Rational(2,5) shouldEqual Rational(9,10)
  }

  it should "subtract numbers correctly" in {
    Rational(3,4) - Rational(1,5) shouldEqual Rational(11,20)
  }

  it should "multiply numbers correctly" in {
    Rational(1,2) * Rational(2,5) shouldEqual Rational(1,5)
  }

  it should "divide numbers correctly" in {
    Rational(1,2) / Rational(5,2) shouldEqual Rational(1,5)
  }

  it should "negate numbers correctly" in {
    -Rational(1,2) shouldEqual Rational(-1,2)
  }

  it should "add and multiply with integers" in {
    1 + Rational(1,2) shouldEqual Rational(3,2)
    5 * Rational(1,2) shouldEqual Rational(5,2)
  }
}
