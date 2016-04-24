/*
 * Copyright 2015 Merlijn Boogerd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.mboogerd.ccrdt.lattice.primitives

import algebra.Order
import algebra.lattice.{BoundedJoinSemilattice, JoinSemilattice}
import com.github.mboogerd.ccrdt.lattice.Chain
import com.github.mboogerd.ccrdt.spireshim._
//import spire.algebra.lattice.{BoundedJoinSemilattice, JoinSemilattice}
import spire.math.{Natural, ULong}

/**
  *
  */
object IntegerLattice {

  import spire.syntax.literals._
  import spire.math.ULong._

  val x: ULong = ul"1"

  val implJSL = implicitly[JoinSemilattice[ULong]]
  val implBJSL = implicitly[BoundedJoinSemilattice[ULong]]

  object Ascending {
    implicit object AscIntegerLattice extends Chain[Int] {
      override def join(lhs: Int, rhs: Int): Int = math.max(lhs, rhs)

      override def compare(x: Int, y: Int): Int = {
        if (x < y)
          -1
        else if(x > y)
          1
        else
          0
      }
    }
  }

  object Descending {
    implicit object DescIntegerLattice extends Chain[Int] {
      override def join(lhs: Int, rhs: Int): Int = math.min(lhs, rhs)
      override def compare(x: Int, y: Int): Int = {
        if (x < y)
          1
        else if(x > y)
          -1
        else
          0
      }
    }
  }
}
