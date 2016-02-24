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

package com.github.mboogerd.ccrdt.syntax

import algebra.lattice.JoinSemilattice
import cats.Order
import cats.Order._
import cats.implicits._
import com.github.mboogerd.ccrdt.VersionVector

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap


/**
  * TODO: Requires refactoring at it does not contain just syntax
  */
trait SemilatticeSyntax {

  implicit def jslSet[S] = new JoinSemilattice[Set[S]] {
    override def join(lhs: Set[S], rhs: Set[S]): Set[S] = lhs union rhs
  }

  implicit def jslTuple[S: JoinSemilattice, T: JoinSemilattice] = new JoinSemilattice[(S, T)] {
    val joinS: (S, S) => S = implicitly[JoinSemilattice[S]].join
    val joinT: (T, T) => T = implicitly[JoinSemilattice[T]].join
    override def join(lhs: (S, T), rhs: (S, T)): (S, T) = (joinS(lhs._1, rhs._1), joinT(lhs._2, rhs._2))
  }

  implicit def jslVersionVector[S: Order] = new JoinSemilattice[VersionVector[S]] {
    override def join(lhs: VersionVector[S], rhs: VersionVector[S]): VersionVector[S] = lhs merge rhs
  }

  implicit def jslSortedMap[K: Order, V: JoinSemilattice] = new JoinSemilattice[SortedMap[K, V]] {
    override def join(lhs: SortedMap[K, V], rhs: SortedMap[K, V]): SortedMap[K, V] = {

      def max = implicitly[JoinSemilattice[V]].join _

      /* TODO: Test the performance hit from generalizing this operation further into some dual (ordered) fold */
      @tailrec
      def iterate(s1: SortedMap[K, V], s2: SortedMap[K, V], aggr: SortedMap[K, V]): SortedMap[K, V] = {
        if (s1.nonEmpty && s2.nonEmpty) {
          val (t1, c1) = s1.head
          val (t2, c2) = s2.head
          val sOrder = t1 compare t2

          if (sOrder == 0) {
            // This elemens exists only in both maps, take the maximum value
            val newAggr = aggr.updated(t1, max(c1, c2))
            iterate(s1.tail, s2.tail, newAggr)
          } else if (sOrder < 0) {
            // This element exists only in s1
            val newAggr = aggr.updated(t1, c1)
            iterate(s1.tail, s2, newAggr)
          } else {
            // This element exists only in s2
            val newAggr = aggr.updated(t2, c2)
            iterate(s1, s2.tail, newAggr)
          }
        } else if (s1.isEmpty)
          aggr ++ s2
        else
          aggr ++ s1
      }

      iterate(lhs, rhs, SortedMap.empty[K, V])
    }
  }

}
