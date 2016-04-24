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
import algebra.lattice.BoundedJoinSemilattice
import com.github.mboogerd.ccrdt.lattice.{BoundedChain, Chain}
import spire.math.Natural


object NaturalLattice {

  object Ascending {
    implicit object AscNaturalLattice extends Chain[Int] with BoundedJoinSemilattice[Int] {
      override def join(lhs: Int, rhs: Int): Int = math.max(lhs, rhs)
      override def zero: Int = 0

      override def compare(x: Int, y: Int): Int = {
        if (x < y)
          -1
        else if(x > y)
          1
        else
          0
      }
    }

    implicit object AscNaturalChain extends BoundedChain[Natural] {
      val naturalOrder = implicitly[Order[Natural]]

      override val zero: Natural = Natural(0l)

      override def join(lhs: Natural, rhs: Natural): Natural = naturalOrder.max(lhs, rhs)

      override def compare(x: Natural, y: Natural): Int = naturalOrder.compare(x, y)
    }
  }

  object Descending {
    implicit object DescNaturalLattice extends Chain[Int] with BoundedJoinSemilattice[Int] {
      override def join(lhs: Int, rhs: Int): Int = math.min(lhs, rhs)
      override def zero: Int = Integer.MIN_VALUE

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
