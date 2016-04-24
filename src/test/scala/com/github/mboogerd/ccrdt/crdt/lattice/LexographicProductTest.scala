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

import algebra.laws.{LatticeLaws, OrderLaws}
import com.github.mboogerd.ccrdt.lattice.LatticeComposition._
import com.github.mboogerd.ccrdt.lattice.LatticeSyntax._
import com.github.mboogerd.ccrdt.lattice.primitives._
import com.github.mboogerd.ccrdt.lattice.product._
import org.scalatest.{FunSuiteLike, Matchers}
import org.typelevel.discipline.scalatest.Discipline
import ArbitraryLattices._
/**
  *
  */
class LexographicProductTest extends FunSuiteLike with Discipline with Matchers {

  test("Lexographic Product should work") {
    import NaturalLattice.Ascending._

    val middle = 5 :⊠: 5
    val secondLower = 5 :⊠: 3
    val firstLower = 3 :⊠: 5
    val mixed = 3 :⊠: 10

    middle ⊔ secondLower should equal (5 :⊠: 5)
    middle ⊔ firstLower should equal (5 :⊠: 5)
    middle ⊔ mixed should equal (5 :⊠: 5)
  }

  test("Lexographic Product should be bounded when both operands are bounded") {
    import NaturalLattice.Ascending._
    (5 :⊠: 5).bottom should equal(0 :⊠: 0)
  }

  def lexProductJSLLaws = {
    implicit val ascInt = NaturalLattice.Ascending.AscNaturalLattice
    implicit val arbNat = arbitraryNat
    LatticeLaws[Int :⊠: Int].boundedJoinSemilattice
  }

  def lexProductOrderLaws = {
    implicit val ascInt = NaturalLattice.Ascending.AscNaturalLattice
    implicit val arbNat = arbitraryNat
    OrderLaws[Int :⊠: Int].order
  }

  checkAll("Lexographic Product JSL", lexProductJSLLaws)
  checkAll("Lexographic Product Order", lexProductOrderLaws)
}
