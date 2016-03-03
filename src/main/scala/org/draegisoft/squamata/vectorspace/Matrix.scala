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

trait Matrix[A <: Field[A]] {
  val dim: Int
  def apply(row: Int): Vector[A]
  def apply(row: Int, column: Int): A

  def +(that: Matrix[A]): Matrix[A]
  def -(that: Matrix[A]): Matrix[A]
  def *(that: Matrix[A]): Matrix[A] 
  def *(that: Vector[A]): Vector[A]

  /**
   * The transpose of the matrix
   **/
  def unary_~ : Matrix[A]
  def unary_- : Matrix[A]

  def inverse : Option[Matrix[A]]

  def scaleBy(scalar: A): Matrix[A]
  def updated(index: Int, vector: Vector[A]): Matrix[A]
  def updated(row: Int, column: Int, elem: A): Matrix[A]
}
