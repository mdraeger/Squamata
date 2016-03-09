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

package org.draegisoft.squamata.examples.hill

import org.draegisoft.squamata.vectorspace.{Matrix, Vector, DenseMatrix, DenseVector}

object HillCipher {
  def encrypt(msg: String, key: Matrix[GF65536]): List[Vector[GF65536]] = {
    val encodedMsg = msg.map(c => GF65536(c.toShort))
                        .grouped(key.dim)
                        .map(v => v ++ padding(key.dim - v.length))
                        .map(s => DenseVector(s.toVector))
                        .toList
    encodedMsg map (_ * key)
  }

  def decrypt(cipher: List[Vector[GF65536]], key: Matrix[GF65536]): String = {
    val decrypted = cipher.map(_*key)
    val msg = decrypted.foldLeft(List.empty[GF65536])((z, v) => z ++ (0 until key.dim)
                       .map(i => v(i)))
                       .map(_.intValue.toChar)
                       .mkString
    msg
  }

  def genKeyPair(dim: Int): (Matrix[GF65536], Matrix[GF65536]) = {
    val rng = new scala.util.Random()
    var keyMatrix: Matrix[GF65536] = DenseMatrix.zeros(dim)
    var invMatrixOpt: Option[Matrix[GF65536]] = None
    while (invMatrixOpt == None) {
      for (i <- 0 until dim; j <- 0 until dim) {
        keyMatrix = keyMatrix.updated(i,j,GF65536(rng.nextInt(32768)))
      }
      invMatrixOpt = keyMatrix.inverse
    }
    (keyMatrix, invMatrixOpt.get)
  }

  private def padding(length: Int) = (0 until length).map(i => GF65536(32)).toVector

  def main(args: Array[String]) = {
    val msg = "This is my very secret hello world message!"
    val (encKey, decKey) = genKeyPair(5)
    val encrypted = encrypt(msg, encKey)
    println(decrypt(encrypted, decKey))
  }
}
