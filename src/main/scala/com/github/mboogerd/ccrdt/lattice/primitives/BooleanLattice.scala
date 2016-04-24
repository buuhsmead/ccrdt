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

import algebra.Eq
import algebra.lattice.BoundedJoinSemilattice

/**
  *
  */
object BooleanLattice {
  object Ascending {
    implicit val ascBooleanLattice = new AscBooleanLattice

    class AscBooleanLattice extends BoundedJoinSemilattice[Boolean] with Eq[Boolean]  {
      override def join(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
      override def zero: Boolean = false

      override def eqv(x: Boolean, y: Boolean): Boolean = x == y
    }
  }

  object Descending {
    implicit val descBooleanLattice = new DescendingBooleanLattice

    class DescendingBooleanLattice extends BoundedJoinSemilattice[Boolean] with Eq[Boolean] {
      override def join(lhs: Boolean, rhs: Boolean): Boolean = lhs && rhs
      override def zero: Boolean = true

      override def eqv(x: Boolean, y: Boolean): Boolean = x == y
    }
  }
}