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

import java.util

import algebra.lattice.JoinSemilattice
import cats.Order
import cats.Order._
import com.github.mboogerd.ccrdt._
import com.github.mboogerd.ccrdt.crdt.DeltaORSet._
import com.github.mboogerd.ccrdt.syntax.semilatticeSyntax._

import scala.collection.immutable.SortedMap
import scala.collection.JavaConversions._

object DeltaORSet {

  sealed trait DeltaOREntry[N]

  case class ObservedEntry[N](addReplica: N, addVersion: Long) extends DeltaOREntry[N]

  case class RemovedEntry[N](addReplica: N, addVersion: Long, remReplica: N, remVersion: Long) extends DeltaOREntry[N]

  type Entries[S, N] = (Set[ObservedEntry[N]], Set[RemovedEntry[N]])
  type MapEntries[S, N] = (S, Entries[S, N])


  /**
    * Evidence that a Delta Observed-Removed set is a Join Semilattice
    * @tparam S
    * @tparam N
    * @return
    */
  implicit def jslDeltaORSet[S: Order, N: Order]: JoinSemilattice[DeltaORSet[S, N]] = new JoinSemilattice[DeltaORSet[S, N]] {
    val joinMap = implicitly[JoinSemilattice[SortedMap[S, (Set[ObservedEntry[N]], Set[RemovedEntry[N]])]]].join _
    val joinVV = implicitly[JoinSemilattice[VersionVector[N]]].join _

    override def join(lhs: DeltaORSet[S, N], rhs: DeltaORSet[S, N]): DeltaORSet[S, N] = {
      DeltaORSet[S, N](joinMap(lhs.data, rhs.data), joinVV(lhs.versionVector, rhs.versionVector))
    }
  }


  def apply[S: Order, N: Order](): DeltaORSet[S, N] = DeltaORSet[S, N](
    SortedMap.empty[S, (Set[ObservedEntry[N]], Set[RemovedEntry[N]])],
    VersionVector[N](SortedMap.empty[N, Long]))
}

/**
  * This is an implementation of the algorithms described in:
  * `A scalable conflict-free replicated set data type` - ISBN: 9780769550008
  *
  * @param data The initial dataset
  * @param versionVector The initial version vector
  * @param ev$1 An ordering relation for the contained type
  * @param ev$2 An ordering relation for the node type in the version vector
  * @tparam S The contained type for this set
  * @tparam N The type of node identifier used in the version vector
  */
case class DeltaORSet[S: Order, N: Order](private[ccrdt] val data: SortedMap[S, (Set[ObservedEntry[N]], Set[RemovedEntry[N]])],
                                          private[ccrdt] val versionVector: VersionVector[N]) extends java.lang.Iterable[S] {

  private val emptyMutation = (Set.empty[ObservedEntry[N]], Set.empty[RemovedEntry[N]])

  type Repr[T, V] = DeltaORSet[T, V]

  /**
    * Add `elem` to this set under the name of `node`
    *
    * @param node
    * @param elem
    * @return A new Delta ORSet with `elem` added
    */
  def add(node: N, elem: S): Repr[S, N] = {
    val newVV = versionVector + node
    val addition = ObservedEntry(node, newVV.version(node))
    val (additions, removals) = mutations(elem)
    val newData = data.updated(elem, (additions + addition, removals))

    DeltaORSet(newData, newVV)
  }

  /**
    * Removes `elem` from this set under the name of `node`
    *
    * @param node
    * @param elem
    * @return A new Delta ORSet with `elem` removed
    */
  def remove(node: N, elem: S): Repr[S, N] = {
    val newVV = versionVector + node
    val newClock = versionVector.version(node) + 1
    val (additions, removals) = mutations(elem)
    val newRemovals = additions.map((or) => RemovedEntry(or.addReplica, or.addVersion, node, newClock))
    val newData = data.updated(elem, (additions, removals ++ newRemovals))

    DeltaORSet(newData, newVV)
  }

  /**
    * Removes all supplied elements from this set
    *
    * @param node
    * @param elems
    * @return A new Delta ORSet with all of `elems` removed
    */
  def removeAll(node: N, elems: Traversable[S]): Repr[S, N] =
    elems.foldLeft(this) { case (cleaner, remove) => cleaner.remove(node, remove) }


  /**
    * Whether the given `elem` is currently in this set
    *
    * @param elem
    * @return true if there exist
    */
  def contains(elem: S): Boolean = {
    val (additions, removals) = mutations(elem)
    !undefined(additions, removals)
  }

  /* TODO: Performance test this implementation vs. more generic semilattice join operation */
  def merge(that: DeltaORSet[S, N]): Repr[S, N] = {
    val mergedDiff = that.data.map[MapEntries[S, N], SortedMap[S, Entries[S, N]]] {
      case (elem, (observed, removals)) =>
        val deltaObserved = observed.filter(obs => versionVector.version(obs.addReplica) < obs.addVersion)
        val deltaRemovals = removals.filter(rem => versionVector.version(rem.remReplica) < rem.remVersion)
        val (localObserved, localRemoved) = mutations(elem)
        (elem, (localObserved ++ deltaObserved, localRemoved ++ deltaRemovals))
    }(sortedMapBuilder)
    val localOnly = data.filter { case (s, _) => !that.data.contains(s) }

    val mergedVVs = versionVector merge that.versionVector

    DeltaORSet(mergedDiff ++ localOnly, mergedVVs)
  }

  /**
    * The additions and removals registered for the given `elem`
    *
    * @param elem
    * @return
    */
  protected def mutations(elem: S): (Set[ObservedEntry[N]], Set[RemovedEntry[N]]) = data.getOrElse(elem, emptyMutation)

  /**
    * Computes whether an entry is effectively removed, given a set of additions and removals for an element
    *
    * @param additions
    * @param removals
    * @return
    */
  private def undefined(additions: Set[ObservedEntry[N]], removals: Set[RemovedEntry[N]]): Boolean = {
    additions.forall(obs => removals.exists(rem =>
      obs.addReplica == rem.addReplica && obs.addVersion == rem.addVersion))
  }

  /**
    * Here we explicitly expose the Java Iterator interface, as the Scala one will enrich the namespace with methods
    * that we should expose with a different signature for CRDTs.
    * @return
    */
  override def iterator(): util.Iterator[S] = data
    .filter { case (s, (additions, removals)) => !undefined(additions, removals) }
    .map { case (s, _) => s }
    .toIterator
}
