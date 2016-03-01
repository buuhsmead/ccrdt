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

package com.github.mboogerd.ccrdt.crdt

import algebra.laws.LatticeLaws
import com.github.mboogerd.CatsSpec
import com.github.mboogerd.ccrdt.crdt.ORSet._
import org.scalacheck.Arbitrary._
import org.scalacheck.Shrink._
import org.scalacheck.{Arbitrary, Gen, Shrink}

import scala.collection.JavaConversions._
import scala.util.Random
/**
 * Created by merlijn on 04/10/15.
 */
object ORSetTest {

  def randInt = () => Random.nextInt()

  implicit def shrinkOREntry[S, P]: Shrink[OREntry[S, P]] = Shrink {
    (value: OREntry[S, P]) => Shrink
      .shrinkTuple3[S, Set[P], Set[P]]
      .shrink(OREntry.unapply(value).get)
      .map { case (s, add, rem) => OREntry(s, add, rem) }
  }

  implicit def shrinkORSet[S, P]: Shrink[ORSet[S, P]] = Shrink {
    (value: ORSet[S, P]) => Shrink
      .shrink(value.data)
      .map(ORSet(_, value.uniqueGen))
  }

  implicit def orSetEntryGen[S, P](genS: Gen[S], genP: Gen[P]): Gen[OREntry[S, P]] = for {
    s <- genS
    added <- Gen.containerOf[Set, P](genP)
    removed <- Gen.containerOf[Set, P](genP)
  } yield OREntry(s, added, removed)


  implicit def orSetArbitrary[S, P](implicit arbS: Arbitrary[S], arbP: Arbitrary[P]): Arbitrary[ORSetLike[S, P]] =
    Arbitrary {
      for {
        sSet <- Gen.containerOf[Set, S](arbS.arbitrary) // Generates unique arbitrary S
        pGen <- arbP.arbitrary
        orEntries <- Gen.sequence(sSet.map(s => orSetEntryGen(s, arbP.arbitrary)))  // Creates unique OREntries from sSet
      } yield ORSet(orEntries.toSet, () => pGen) // <- IDEA no likey, does compile...
    }

}

class ORSetTest extends CatsSpec {

  import ORSetTest._

  checkAll("ORSetLike[Int, Int]", LatticeLaws[ORSetLike[Int, Int]].joinSemilattice)
}
