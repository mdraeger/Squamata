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

class GF2UnitTest extends UnitTest("GF2") {
  it should "define the right constants" in {
    (new GF2(0)).zero shouldEqual GF2(0)
    (new GF2(0)).one  shouldEqual GF2(1)
  }

  it should "deny division by zero" in {
    an [IllegalArgumentException] should be thrownBy {
      GF2(1) / GF2(0)
    }
  }

  it should "add numbers correctly" in {
      GF2(0) + GF2(0) shouldEqual GF2(0)
      GF2(0) + GF2(1) shouldEqual GF2(1)
      GF2(1) + GF2(0) shouldEqual GF2(1)
      GF2(1) + GF2(1) shouldEqual GF2(0)
  }

  it should "subtract numbers correctly" in {
      GF2(0) - GF2(0) shouldEqual GF2(0)
      GF2(0) - GF2(1) shouldEqual GF2(1)
      GF2(1) - GF2(0) shouldEqual GF2(1)
      GF2(1) - GF2(1) shouldEqual GF2(0)
  }

  it should "multiply numbers correctly" in {
      GF2(0) * GF2(0) shouldEqual GF2(0)
      GF2(0) * GF2(1) shouldEqual GF2(0)
      GF2(1) * GF2(0) shouldEqual GF2(0)
      GF2(1) * GF2(1) shouldEqual GF2(1)
  }

  it should "divide numbers correctly" in {
      GF2(0) / GF2(1) shouldEqual GF2(0)
      GF2(1) / GF2(1) shouldEqual GF2(1)
  }

  it should "negate numbers correctly" in {
    -GF2(1) shouldEqual GF2(1)
    -GF2(0) shouldEqual GF2(0)
  }

  it should "add and multiply with integers" in {
    0 + GF2(0) shouldEqual GF2(0)
    0 + GF2(1) shouldEqual GF2(1)
    1 + GF2(0) shouldEqual GF2(1)
    1 + GF2(1) shouldEqual GF2(0)
  }
}
