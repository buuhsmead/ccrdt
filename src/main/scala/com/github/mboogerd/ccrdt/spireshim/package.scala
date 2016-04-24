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

package com.github.mboogerd.ccrdt

import algebra.lattice.{BoundedJoinSemilattice ⇒ AlgebraBoundedJSL, JoinSemilattice ⇒ AlgebraJSL}
import algebra.{Order ⇒ AlgebraOrder, PartialOrder ⇒ AlgebraPartialOrder}
import spire.algebra.lattice.{BoundedJoinSemilattice ⇒ SpireBoundedJSL, JoinSemilattice ⇒ SpireJSL}
import spire.algebra.{Order ⇒ SpireOrder, PartialOrder ⇒ SpirePartialOrder}
/**
  *
  */
package object spireshim {

  /* === ORDER === */
  implicit def orderShimSA[S: SpireOrder]: AlgebraOrder[S] = new AlgebraOrder[S] {
    override def compare(x: S, y: S): Int = implicitly[SpireOrder[S]].compare(x, y)
  }

  implicit def orderShimAS[S: AlgebraOrder]: SpireOrder[S] = new SpireOrder[S] {
    override def compare(x: S, y: S): Int = implicitly[AlgebraOrder[S]].compare(x, y)
  }


  /* === PARTIAL ORDER === */
  implicit def partialOrderShimSA[S: SpirePartialOrder]: AlgebraPartialOrder[S] = new AlgebraPartialOrder[S] {
    override def partialCompare(x: S, y: S): Double = implicitly[SpirePartialOrder[S]].partialCompare(x, y)
  }

  implicit def partialOrderShimAS[S: AlgebraPartialOrder]: SpirePartialOrder[S] = new SpirePartialOrder[S] {
    override def partialCompare(x: S, y: S): Double = implicitly[AlgebraPartialOrder[S]].partialCompare(x, y)
  }


  /* === JOIN SEMI-LATTICE === */
  implicit def jslShimSA[S: SpireJSL]: AlgebraJSL[S] = new AlgebraJSL[S] {
    override def join(lhs: S, rhs: S): S = implicitly[SpireJSL[S]].join(lhs, rhs)
  }
  implicit def jslShimAS[S: AlgebraJSL]: SpireJSL[S] = new SpireJSL[S] {
    override def join(lhs: S, rhs: S): S = implicitly[AlgebraJSL[S]].join(lhs, rhs)
  }

  /* === BOUNDED JOIN SEMI-LATTICE === */
  implicit def jslBoundedShimSA[S: SpireBoundedJSL]: AlgebraBoundedJSL[S] = new AlgebraBoundedJSL[S] {
    val spireBJSL: SpireBoundedJSL[S] = implicitly[SpireBoundedJSL[S]]
    override def join(lhs: S, rhs: S): S = spireBJSL.join(lhs, rhs)
    override def zero: S = spireBJSL.zero
  }
  implicit def jslBoundedShimAS[S: AlgebraBoundedJSL]: SpireBoundedJSL[S] = new SpireBoundedJSL[S] {
    val spireBJSL: AlgebraBoundedJSL[S] = implicitly[AlgebraBoundedJSL[S]]
    override def join(lhs: S, rhs: S): S = spireBJSL.join(lhs, rhs)
    override def zero: S = spireBJSL.zero
  }

}
