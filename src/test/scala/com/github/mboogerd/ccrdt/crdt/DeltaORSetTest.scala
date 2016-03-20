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

import java.lang.Iterable

import algebra.lattice.JoinSemilattice
import cats.Order
import cats.implicits._
import com.github.mboogerd.GenHelper._
import com.github.mboogerd.TestSpec
import com.github.mboogerd.ccrdt.crdt.DeltaORSet._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.enablers.Containing

import scala.collection.JavaConversions._
/**
  *
  */
object DeltaORSetTest {

  // Add a random element at a random node to the given set
  def addRandom[S: Order : Arbitrary, N: Order : Arbitrary](deltaORSet: DeltaORSet[S, N]): Gen[DeltaORSet[S, N]] = for {
    node <- arbitrary[N]
    elem <- arbitrary[S]
  } yield deltaORSet.add(node, elem)

  // Remove a randomly chosen element from this set at a random node
  def removeSome[S: Order : Arbitrary, N: Order : Arbitrary](deltaORSet: DeltaORSet[S, N]): Gen[DeltaORSet[S, N]] = for {
    node <- arbitrary[N]
    some <- Gen.someOf(deltaORSet.data.keySet)
  } yield deltaORSet.removeAll(node, some)

  // Add (80% chance) or remove (20% chance) an arbitrary element
  def modifyDeltaORSet[S: Order : Arbitrary, N: Order : Arbitrary](deltaORSet: DeltaORSet[S, N]): Gen[DeltaORSet[S, N]] =
    Gen.frequency((8, addRandom(deltaORSet)), (2, removeSome(deltaORSet)))

  // Generate a complete delta orset by iteratively adding or removing
  implicit def arbitraryDeltaORSet[S: Order : Arbitrary, N: Order : Arbitrary](implicit deltaORSet: DeltaORSet[S, N]): Arbitrary[DeltaORSet[S, N]] =
    Arbitrary(foldGen(deltaORSet)(modifyDeltaORSet[S, N]))


  case class TestEntry(data: String)

  case class TestNode(address: String)

  implicit val orderTestEntry: Order[TestEntry] = new Order[TestEntry] {
    override def compare(x: TestEntry, y: TestEntry): Int = x.data compareTo y.data
  }
  implicit val orderTestNode: Order[TestNode] = new Order[TestNode] {
    override def compare(x: TestNode, y: TestNode): Int = x.address compareTo y.address
  }
}

class DeltaORSetTest extends TestSpec {

  import DeltaORSetTest._

  implicit val emptyDeltaOrSet: DeltaORSet[String, String] = DeltaORSet[String, String]()

  def stringBasedDORSets = arbitrary[DeltaORSet[String, String]]

  "DeltaORSet" should "initially contain no elements" in {
    val emptyDORSet = DeltaORSet.apply[String, String]()

    forAll(arbitrary[String]) { value =>
      emptyDORSet should not contain value
    }
  }


  it should "contain an element if it is added" in {
    var emptyDORSet = DeltaORSet.apply[String, String]()

    forAll(arbitrary[String], arbitrary[String]) { case (node, value) =>
      val nonEmptyDORSet = emptyDORSet.add(node, value)
      nonEmptyDORSet should contain(value)
    }
  }


  it should "no longer contain an element if it is removed" in {
    val removeGen = for {
      set <- stringBasedDORSets
      node <- Gen.identifier
      values <- Gen.someOf(set.data.keySet)
    } yield (set, node, values)

    forAll(removeGen) { case (set, onNode, values) =>
      val newSet: DeltaORSet[String, String] = values.foldLeft(set) { case (cleaner, remove) => cleaner.remove(onNode, remove) }
      newSet should not contain oneElementOf(values)
    }
  }


  it should "not change when merging with itself" in {
    forAll(stringBasedDORSets) { case set =>
        set merge set should equal(set)
    }
  }

  it should "have a merge implementation that corresponds to Join Semilattice - Least Upperbound" in {
    implicit val arbTestEntry: Arbitrary[TestEntry] = Arbitrary(arbitrary[String] map (TestEntry(_)))
    implicit val arbTestNode: Arbitrary[TestNode] = Arbitrary(arbitrary[String] map (TestNode(_)))
    implicit val emptyDeltaOrSet: DeltaORSet[TestEntry, TestNode] = DeltaORSet[TestEntry, TestNode]()
    val genDORSet = arbitrary[DeltaORSet[TestEntry, TestNode]]

    // It would be interesting to make use of Cats' laws to test the merge function. However, this takes as input
    // a single generator, independently invoked twice to get a pair whose merge can be tested for associativity,
    // commutativity and idempotence. The trouble here is that a Delta ORSet is not a true Join Semilattice;
    // its properties only hold for (delta-orset) pairs whose version-vectors are free of 'collisions' (i.e. there are
    // no two different actions identified using the same node-clock entry).
    //
    // Guaranteeing two 'collision-free' version vectors seems a bit of a hassle; instead, we generate a (possibly
    // empty) base DORSet, draw two disjoint sets of node identifiers, and evolve the base once for each such node-set.
    // Then, we compare whether the speed-optimized merge behaves equivalent to the the easier-to-verify
    // join-semilattice join implementation.
    val disjointGen = for {
      base <- genDORSet
      nodes = base.versionVector.elems.keySet

      // draw some of the nodes, plus some extra
      someSubsetOfNodes <- Gen.someOf(nodes)
      someMore <- Gen.nonEmptyContainerOf(arbitrary[TestNode])
      all = (someSubsetOfNodes ++ someMore).toSet
      // compute the difference, and add some more
      remainingNodes = nodes -- all
      remainingMore <- Gen.nonEmptyContainerOf(arbitrary[TestNode])

      // compute the final nodesets
      firstNodes = (all -- remainingMore).toSeq
      secondNodes = remainingNodes ++ remainingMore

      // create two new orsets from mutating the base on the two sets of nodes
      firstSet <- arbitraryDeltaORSet(orderTestEntry, arbTestEntry,
        orderTestNode, Arbitrary(Gen.oneOf(firstNodes)), base).arbitrary

      secondSet <- arbitraryDeltaORSet(orderTestEntry, arbTestEntry,
        orderTestNode, Arbitrary(Gen.oneOf(remainingNodes.toSeq)), base).arbitrary

    } yield (base, firstSet, secondSet)

    val joinDeltaORSets = implicitly[JoinSemilattice[DeltaORSet[TestEntry, TestNode]]].join _

    forAll(disjointGen) { case (base, firstEvolution, secondEvolution) =>
      val toTest = firstEvolution merge secondEvolution
      val toCompare = joinDeltaORSets(firstEvolution, secondEvolution)
      toTest should equal(toCompare)
    }
  }


  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfig(minSuccessful = 250, maxDiscarded = 300)
}
