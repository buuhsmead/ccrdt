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

import algebra.lattice.JoinSemilattice
import cats.Order
import cats.Order._
import cats.std.long._
import com.github.mboogerd.GenHelper._
import com.github.mboogerd.TestSpec
import com.github.mboogerd.ccrdt.syntax.all._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpecLike, Matchers}

import scala.collection.JavaConversions._
import scala.collection.immutable.SortedMap
/**
  *
  */
object VersionVectorTest {

  /* ADT evidence */

  implicit def sortedMapJSL[K: Order, V: Order]: JoinSemilattice[SortedMap[K, V]] = new JoinSemilattice[SortedMap[K, V]] {
    val keyOrder = implicitly[Order[K]].compare _

    override def join(lhs: SortedMap[K, V], rhs: SortedMap[K, V]): SortedMap[K, V] = {

      def joinElements(left: Seq[(K, V)], right: Seq[(K, V)]): Seq[(K, V)] =
        (left.headOption, right.headOption) match {
          case (Some((k1, v1)), Some((k2, v2))) =>
            val order = keyOrder(k1, k2)
            if (order == 0)
              (k1 -> max(v1, v2)) +: joinElements(left.tail, right.tail)
            else if (order > 0)
              (k2 -> v2) +: joinElements(left, right.tail)
            else
              (k1 -> v1) +: joinElements(left.tail, right)
          case (None, None) => Nil
          case (_, None) => left
          case (None, _) => right
        }
      SortedMap(joinElements(lhs.toSeq, rhs.toSeq): _*)
    }
  }

  implicit def versionVectorJSL[K: Order]: JoinSemilattice[VersionVector[K]] = new JoinSemilattice[VersionVector[K]] {
    override def join(lhs: VersionVector[K], rhs: VersionVector[K]): VersionVector[K] = {
      VersionVector(sortedMapJSL[K, Long].join(lhs.elems, rhs.elems))
    }
  }

  implicit object VersionVectorJSL extends JoinSemilattice[VersionVector[String]] {
    override def join(lhs: VersionVector[String], rhs: VersionVector[String]): VersionVector[String] =
      versionVectorJSL[String].join(lhs, rhs)
  }


  /* Generators */
  val versionVectorTuple: Gen[(String, Long)] = for {
    id <- Gen.identifier
    clock <- Gen.choose(0l, Long.MaxValue)
  } yield (id, clock)

  def versionVectorDataGen[T: Order](gen: Gen[(T, Long)]): Gen[SortedMap[T, Long]] = for {
    tupleMap <- Gen.mapOf(gen)
  } yield SortedMap(tupleMap.toArray: _*)

  def nonEmptyVersionVectorDataGen[T: Order](gen: Gen[(T, Long)]): Gen[SortedMap[T, Long]] = for {
    tupleMap <- Gen.nonEmptyMap(gen)
  } yield SortedMap(tupleMap.toArray: _*)


  /**
    * Progresses at least one node in the version vector by incrementing its clock.
    *
    * @param vv
    * @return
    */
  def progressClocks(vv: VersionVector[String]): Gen[VersionVector[String]] = for {
    some <- atLeastOne(vv.elems.toSet)
    incs <- Gen.sequence(some.map { case (s, l) => Gen.choose(l + 1, Long.MaxValue) map (s -> _) })
  } yield vv join VersionVector(SortedMap(incs: _*))


  /**
    * Attempts to add an additional node to the version vector. This is not guaranteed to succeed as a the newly
    * generated version vector may have a subset of the keys of the input version vector. In that case, the generated
    * version vector is guaranteed to be the same as the input vector.
    *
    * @param vv
    * @return
    */
  def progressNodes(vv: VersionVector[String]): Gen[VersionVector[String]] =
    nonEmptyVersionVector.retryUntil { case vv2 => vv2.elems.keySet.intersect(vv.elems.keySet).isEmpty } map (_ join vv)

  def progressVersionVector(vv: VersionVector[String]): Gen[VersionVector[String]] = for {
    functions <- atLeastOne(Set(progressClocks(vv), progressNodes(vv)))
    genSeq <- Gen.sequence(functions)
    modified = genSeq.foldLeft(vv) { case (vvNew, vvPrev) => vvPrev join vvNew }
  } yield modified

  def versionVectorGen[T: Order](gen: Gen[SortedMap[T, Long]]): Gen[VersionVector[T]] = gen.map(VersionVector(_))

  implicit val arbitraryVersionVector: Arbitrary[VersionVector[String]] =
    Arbitrary(versionVectorGen(versionVectorDataGen(versionVectorTuple)))

  val nonEmptyVersionVector: Gen[VersionVector[String]] =
    versionVectorGen(nonEmptyVersionVectorDataGen(versionVectorTuple))

}

class VersionVectorTest extends TestSpec {

  import VersionVectorTest._


  "VersionVector" should "determine equivalence" in {
    forAll(arbitrary[VersionVector[String]]) { versionVector =>
      versionVector == versionVector should equal(true)
    }
  }

  it should "correctly determine order when the 'comparee' has progressed" in {
    val vvWithProgression = withDerivative(nonEmptyVersionVector)(vvs => progressClocks(vvs))

    forAll(vvWithProgression) { case (oldVV, progressedVV) =>
      oldVV < progressedVV should equal(true)
      progressedVV > oldVV should equal(true)
    }
  }

  it should "correctly determine order when the 'comparee' has additional nodes" in {
    val vvWithJoinedNodes = withDerivative(arbitrary[VersionVector[String]])(progressNodes)

    forAll(vvWithJoinedNodes) { case (fewerNodes, moreNodes) =>
      whenever(moreNodes.size > fewerNodes.size) {
        fewerNodes < moreNodes should equal(true)
        moreNodes > fewerNodes should equal(true)
      }
    }
  }

  it should "detect concurrency when both have progressed in a diverging manner" in {

    // Naive (inefficient) check to see whether two version vectors are concurrent
    def isConcurrent[T](vv1: VersionVector[T], vv2: VersionVector[T]): Boolean =
      (vv1.elems.exists { case (s, l) => l > vv2.elems.getOrElse(s, 0l) } &&
        vv2.elems.exists { case (s, l) => l > vv1.elems.getOrElse(s, 0l) }) ||
        (vv2.elems.exists { case (s, l) => l > vv1.elems.getOrElse(s, 0l) } &&
          vv1.elems.exists { case (s, l) => l > vv2.elems.getOrElse(s, 0l) })

    // progress two version vectors from a common non-empty ancestor. High probability (but no certainty) that they are concurrent
    val twoProgressions = withDerivative(nonEmptyVersionVector)(vvs => for {
      first <- progressVersionVector(vvs)
      second <- progressVersionVector(vvs)
    } yield (first, second)).map(_._2)

    forAll(twoProgressions) { case (vv1, vv2) =>
      whenever(isConcurrent(vv1, vv2)) {
        vv1 <> vv2 should be(true)
      }
    }
  }

  it should "allow incrementing a particular node" in {
    val vvWithChosenId = withDerivative(arbitrary[VersionVector[String]])((vvs) => for {
      newId <- Gen.identifier
      chosenId <- Gen.oneOf(vvs.elems.toSeq) map (_._1)
      newOrChoice <- Gen.oneOf(newId, chosenId)
    } yield newOrChoice)


    forAll(vvWithChosenId) { case (vv, id) =>
      val oldClock = vv.version(id)
      val newVV = vv + id
      val newClock = newVV.version(id)

      newClock should equal(oldClock + 1)
    }
  }

  it should "have order properties that are completely disjoint" in {
    val vvPair = for {
      base <- arbitrary[VersionVector[String]]
      leftModification <- arbitrary[VersionVector[String]]
      rightModification <- arbitrary[VersionVector[String]]
    } yield (base join leftModification, base join rightModification)

    forAll(vvPair) { case (vv1, vv2) =>
      val earlier = vv1 < vv2
      val later = vv1 > vv2
      val concurrent = vv1 <> vv2
      val same = vv1 == vv2

      val flags = List(earlier, later, concurrent, same) filter identity
      flags should have size 1
    }
  }

  it should "be able to merge" in {
    forAll(arbitrary[VersionVector[String]], arbitrary[VersionVector[String]]) { case (vv1, vv2) =>
      // Here we compare the native (optimized) merge vs. a more intuitive implementation in the join-semilattice evidence above.
      vv1.merge(vv2) should equal(vv1.join(vv2))
    }
  }

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfig(minSuccessful = 250)
}
