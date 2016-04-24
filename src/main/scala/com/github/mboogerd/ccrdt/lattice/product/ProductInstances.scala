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

package com.github.mboogerd.ccrdt.lattice.product

import algebra.Eq
import algebra.lattice.{BoundedJoinSemilattice, JoinSemilattice}
import com.github.mboogerd.ccrdt.lattice.LatticeSyntax._
/**
  *
  */
object ProductInstances extends ProductSyntax {

  class ProductJoinSemilattice[S: JoinSemilattice: Eq, T: JoinSemilattice: Eq] extends JoinSemilattice[S :×: T] with Eq[S :×: T] {
    override def join(lhs: S :×: T, rhs: S :×: T): S :×: T =
      lhs.left ⊔ rhs.left :×: lhs.right ⊔ rhs.right

    override def eqv(x: S :×: T, y: S :×: T): Boolean = {
      val leftEq = implicitly[Eq[S]].eqv(x.left, y.left)
      val rightEq = implicitly[Eq[T]].eqv(x.right, y.right)
      leftEq && rightEq
    }
  }

  class ProductBoundedJoinSemilattice[S: BoundedJoinSemilattice: Eq, T: BoundedJoinSemilattice: Eq] extends ProductJoinSemilattice[S, T] with BoundedJoinSemilattice[S :×: T] {
    override def zero: S :×: T =
      implicitly[BoundedJoinSemilattice[S]].zero :×: implicitly[BoundedJoinSemilattice[T]].zero
  }
}
