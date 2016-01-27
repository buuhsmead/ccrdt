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

import algebra.CommutativeGroup
import algebra.lattice.JoinSemilattice
import cats._
import cats.implicits._
import com.github.mboogerd.ccrdt.ORSet._

import scala.language.reflectiveCalls

/**
  * Created by merlijn on 03/10/15.
 *
 * ORSet = Set[OREntry]
 * DerivedORSetLike(functional, set operations)
 * MutableORSetLike (add / remove) extends DerivedORSetLike
 *
 */

trait ORSetLike[S, U] {

  type Repr[T, V] <: ORSetLike[T, V]

  def data: Set[OREntry[S, U]]

  /* Core API */
  def add(elem: S): Repr[S, U]

  def remove(elem: S): Repr[S, U]

  /**
   * Monotonic read operation; this operation does not return until the value of x is greater than or equal to v in the
   * partial order relation induced over x at which time the operation returns the current value of x
 *
   * @param v
   */
  def read(v: S): S

  /**
   * Same as read(v) except that it waits until the value of x is strictly greater than v
 *
   * @param v
   * @return
   */
  def strict_read(v: S): S


  /* Functional operators */
  def map[T](f: (S) => T): Repr[T, U]

  def filter(p: (S) => Boolean): Repr[S, U]

  /**
   * An implementation of foldLeft that works only for operations that are associative, commutative and that have an
   * inverse op'
 *
   * @param z
   * @param op An associative, commutative function with inverse (closest: CommutativeGroup?)
   * @tparam T
   * @return
   */
  def foldLeft[T](z: T)(op: (T, S) => T): T


  /* Set theoretic operators */
  def union(that: ORSetLike[S, U]): Repr[S, U]

  def intersect(that: ORSetLike[S, U]): Repr[S, (U, U)]

  def product[T](that: ORSetLike[T, U]): Repr[(S, T), (U, U)]

}




object ORSet {

  case class OREntry[S, U](elem: S, added: Set[U] = Set.empty[U], removed: Set[U] = Set.empty[U])

  /* FIXME: temporary fix waiting for https://github.com/non/cats/pull/497 to become mainstream */
  implicit def setMonoid[A]: Monoid[Set[A]] = MonoidK[Set].algebra[A]

  implicit def orSetJoinSemilattice[S, P]: JoinSemilattice[ORSetLike[S, P]] =
    new JoinSemilattice[ORSetLike[S, P]] {
      override def join(lhs: ORSetLike[S, P], rhs: ORSetLike[S, P]): ORSetLike[S, P] = lhs union rhs
    }

  implicit def orSetEq[S, P]: Eq[ORSetLike[S, P]] = new Eq[ORSetLike[S, P]] {
    override def eqv(x: ORSetLike[S, P], y: ORSetLike[S, P]): Boolean = x.data == y.data
  }

  implicit class Joinable[T](set1: Set[T]) {
    def join(set2: Set[T]) = new {
      def on[K](prop: T => K): Set[(K, Set[T], Set[T])] = {
        val lhs = set1.groupBy(prop)
        val rhs = set2.groupBy(prop)
        (lhs.keySet & rhs.keySet) map (key => (key, lhs(key), rhs(key)))
      }
    }
  }

  implicit class Crossable[X](xs: Set[X]) {
    def cross[Y](ys: Set[Y]) = for {x <- xs; y <- ys} yield (x, y)
  }

  type UID[U] = () => U
}





trait MutableORSetLike[S, U] extends CommutativeGroup[S] {

}
trait FunctorORSetLike[S, U] extends Functor[({type λ[α] = FunctorORSetLike[α, U]})#λ] {
  override def map[A, B](fa: FunctorORSetLike[A, U])(f: (A) => B): FunctorORSetLike[B, U] = ???
}


case class ORSet[S, U](data: Set[OREntry[S, U]], uniqueGen: UID[U]) extends ORSetLike[S, U] {

  assert(data.map(_.elem).size == data.size, "ORSet data may not contain OREntries with duplicate `elem` attribute values")


  override type Repr[T, V] = ORSet[T, V]

  /* Core API */
  override def add(elem: S): Repr[S, U] = {
    val unique = uniqueGen()
    val split = data.partition(_.elem == elem)
    val newEntry = split._1
      .map{ case OREntry(`elem`, added, removed) => OREntry(elem, added + unique, removed) }
      .headOption
      .getOrElse(OREntry(elem, Set(unique)))

    ORSet(split._2 + newEntry, uniqueGen)
  }

  override def remove(elem: S): Repr[S, U] = {
    val unique = uniqueGen
    val split = data.partition(_.elem == elem)
    val newEntry = split._1
      .map{ case OREntry(`elem`, added, removed) => OREntry(elem, added, added ++ removed) }

    ORSet(split._2 ++ newEntry, uniqueGen)
  }

  /**
   * Same as read(v) except that it waits until the value of x is strictly greater than v
 *
   * @param v
   * @return
   */
  override def strict_read(v: S): S = ???

  /**
   * Monotonic read operation; this operation does not return until the value of x is greater than or equal to v in the
   * partial order relation induced over x at which time the operation returns the current value of x
 *
   * @param v
   */
  override def read(v: S): S = ???


  /* Functional operators */
  override def map[T](f: (S) => T): Repr[T, U] = {
    val applied = data.groupBy(s => f(s.elem))
    val orEntries = applied.map{ case (mappedValue, entries) =>
      OREntry(mappedValue, entries.flatMap(_.added), entries.flatMap(_.removed))
    }
    ORSet(orEntries.toSet, uniqueGen)
  }

  override def filter(p: (S) => Boolean): Repr[S, U] = {
    val split = data.partition(p compose(_.elem))
    val kept = split._1
    val filtered = split._2 map { case OREntry(elem, added, removed) => OREntry(elem, added, added ++ removed)}
    ORSet(kept ++ filtered, uniqueGen)
  }

  override def foldLeft[T](z: T)(op: (T, S) => T): T = ??? //TODO: CommutativeGroup properties on S?


  /* Set theoretic operators */
  override def union(that: ORSetLike[S, U]): Repr[S, U] = {
    val joined = this.data join that.data on (_.elem)

    val orEntries = joined.foldLeft(Set.empty[OREntry[S, U]]) { case (aggr, (key, orEntry1, orEntry2)) =>
      aggr + OREntry(key,
        orEntry1.flatMap(_.added) ++ orEntry2.flatMap(_.added),
        orEntry1.flatMap(_.removed) ++ orEntry2.flatMap(_.removed))
    }

    ORSet(orEntries, uniqueGen)
  }

  override def intersect(that: ORSetLike[S, U]): Repr[S, (U, U)] = {
    val joined = this.data join that.data on (_.elem)
    val fullJoined = joined.filter { case (_, or1, or2) => or1.nonEmpty && or2.nonEmpty }

    val orEntries = fullJoined.foldLeft(Set.empty[OREntry[S, (U, U)]]) { case (aggr, (key, orEntry1, orEntry2)) =>
      aggr + OREntry(key,
        orEntry1.flatMap(_.added) cross orEntry2.flatMap(_.added),
        orEntry1.flatMap(_.removed) cross orEntry2.flatMap(_.removed))
    }

    ORSet(orEntries, tupledGen(uniqueGen))
  }

  override def product[T](that: ORSetLike[T, U]): Repr[(S, T), (U, U)] = {
    val orEntries = for {
      t1 <- this.data
      t2 <- that.data
    } yield OREntry((t1.elem, t2.elem), t1.added cross t2.added,
        (t1.added cross t2.removed) union (t1.removed cross t2.added))

    ORSet(orEntries, tupledGen(uniqueGen))
  }

  /**
   * FIXME: Dirty fix to align the types for intersect and product types. Requires revising the ORSet type I guess?
 *
   * @param baseGen
   * @return
   */
  private def tupledGen(baseGen: UID[U]): UID[(U, U)] = () => (baseGen(), baseGen())

}

//
//class ORSetStream[S, U] extends ORSetLike[S, U] {
//  override def data: Set[OREntry[S, U]] = ???
//
//  /**
//   * An implementation of foldLeft that works only for operations that are associative, commutative and that have an
//   * inverse op'
//   * @param z
//   * @param op An associative, commutative function with inverse
//   * @tparam T
//   * @return
//   */
//  override def foldLeft[T](z: T)(op: (T, S) => T): T = ???
//
//  override def filter(p: (S) => Boolean): ORSetLike[S, U] = ???
//
//  override def product[T](that: ORSetLike[T, U]): ORSetLike[(S, T), (U, U)] = ???
//
//  override def intersect(that: ORSetLike[S, U]): ORSetLike[S, U] = ???
//
//  override def remove(elem: S): ORSetLike[S, U] = ???
//
//  /**
//   * Same as read(v) except that it waits until the value of x is strictly greater than v
//   * @param v
//   * @return
//   */
//  override def strict_read(v: S): S = ???
//
//  /**
//   * Monotonic read operation; this operation does not return until the value of x is greater than or equal to v in the
//   * partial order relation induced over x at which time the operation returns the current value of x
//   * @param v
//   */
//  override def read(v: S): S = ???
//
//  /* Core API */
//  override def add(elem: S): ORSetLike[S, U] = ???
//
//  /* Set theoretic operators */
//  override def union(that: ORSetLike[S, U]): ORSetLike[S, U] = ???
//
//  /* Functional operators */
//  override def map[T](f: (S) => T): ORSetLike[T, U] = ???
//}
