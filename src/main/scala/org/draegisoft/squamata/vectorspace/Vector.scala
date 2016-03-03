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

import org.draegisoft.squamata.field.Field

trait Vector[A <: Field[A]] {
  def apply(index: Int): A
  val dim: Int

  def +(that: Vector[A]): Vector[A] 
  def -(that: Vector[A]): Vector[A] 
  def *(that: Vector[A]): A
  def *(that: Matrix[A]): Vector[A]
  def unary_- : Vector[A]

  def scaleBy(scalar: A): Vector[A]
  def updated(index: Int, scalar: A): Vector[A]
}
