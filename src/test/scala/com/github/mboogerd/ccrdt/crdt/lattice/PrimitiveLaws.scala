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

package com.github.mboogerd.ccrdt.crdt.lattice

import algebra.laws.LatticeLaws
import com.github.mboogerd.CatsSpec
import com.github.mboogerd.ccrdt.crdt.lattice.ArbitraryLattices._
import com.github.mboogerd.ccrdt.lattice.primitives.SingletonLattice._
import com.github.mboogerd.ccrdt.lattice.primitives._
import org.scalacheck.{Arbitrary, Gen}
import spire.math.{Natural, UInt, ULong}
import com.github.mboogerd.ccrdt.spireshim._
/**
  *
  */

class PrimitiveLaws extends CatsSpec {

  checkAll("⊥", LatticeLaws[⊥.type].boundedJoinSemilattice)

  checkAll("Boolean", LatticeLaws[Boolean].boundedJoinSemilattice)

  checkAll("Int.ascending", LatticeLaws[Int].joinSemilattice(IntegerLattice.Ascending.AscIntegerLattice))
  checkAll("Int.descending", LatticeLaws[Int].joinSemilattice(IntegerLattice.Descending.DescIntegerLattice))
  checkAll("Non-negative-Int.ascending", LatticeLaws[Int].joinSemilattice(NaturalLattice.Ascending.AscNaturalLattice))
  checkAll("Non-negative-Int.descending", LatticeLaws[Int].joinSemilattice(NaturalLattice.Descending.DescNaturalLattice))


  def uInt = {
    import UInt._
    implicit val genUInt = Arbitrary(Gen.choose(0, Int.MaxValue) map UInt.apply)
    LatticeLaws[UInt].boundedJoinSemilattice
  }

  def uLong = {
    import ULong._
    implicit val genUInt = Arbitrary(Gen.choose(0l, Long.MaxValue) map ULong.apply)
    LatticeLaws[ULong].boundedJoinSemilattice
  }

  def natural = {
    import NaturalLattice.Ascending.AscNaturalChain
    implicit val genUInt = Arbitrary(Gen.choose(0l, Long.MaxValue) map Natural.apply)
    LatticeLaws[Natural].boundedJoinSemilattice
  }

  checkAll("UInt.ascending", uInt)
  checkAll("ULong.ascending", uInt)
  checkAll("ℕ.ascending", uInt)
}
