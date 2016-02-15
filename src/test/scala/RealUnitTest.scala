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

class RealUnitTest extends UnitTest("Real") {
  val doubles = List(0.0, 1.0, math.Pi, math.E, math.sqrt(2), -1.0, -5.0)

  it should "define the right constants" in {
    Real(0).zero shouldEqual Real(0)
    Real(0).one  shouldEqual Real(1)
  }

  it should "deny division by zero" in {
    an [IllegalArgumentException] should be thrownBy {
      Real(1) / Real(0)
    }
  }

  it should "add numbers correctly" in {
    for (x <- doubles; y <- doubles) 
      Real(x) + Real(y) shouldEqual Real(x+y)
  }

  it should "subtract numbers correctly" in {
    for (x <- doubles; y <- doubles) 
      Real(x) - Real(y) shouldEqual Real(x-y)
  }

  it should "multiply numbers correctly" in {
    for (x <- doubles; y <- doubles) 
      Real(x) * Real(y) shouldEqual Real(x*y)
  }

  it should "divide numbers correctly" in {
    for (x <- doubles; y <- doubles filter ( _ != 0.0)) 
      Real(x) / Real(y) shouldEqual Real(x/y)
  }

  it should "negate numbers correctly" in {
    for (x <- doubles; y <- doubles) 
      -Real(x) shouldEqual Real(-x)
  }

  it should "add and multiply with integers" in {
    for (x <- doubles) 
      1+Real(x) shouldEqual Real(x + 1)
    for (x <- doubles) 
      5*Real(x) shouldEqual Real(x * 5)
  }
}
