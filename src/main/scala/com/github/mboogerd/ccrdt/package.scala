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

package com.github.mboogerd

import cats._

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import cats.Order._

package object ccrdt {

  implicit class JoinableSet[T](set1: Set[T]) {
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


  def sortedMapBuilder[K: Order, V] = new CanBuildFrom[SortedMap[K, V], (K, V), SortedMap[K, V]] {
    val empty = SortedMap.empty[K, V]

    override def apply(from: SortedMap[K, V]): mutable.Builder[(K, V), SortedMap[K, V]] = new mutable.Builder[(K, V), SortedMap[K, V]] {
      var map: SortedMap[K, V] = from

      override def +=(elem: (K, V)): this.type = {
        map = map.updated(elem._1, elem._2)
        this
      }

      override def result(): SortedMap[K, V] = map

      override def clear(): Unit = map = empty
    }

    override def apply(): mutable.Builder[(K, V), SortedMap[K, V]] = new mutable.Builder[(K, V), SortedMap[K, V]] {
      var map: SortedMap[K, V] = empty

      override def +=(elem: (K, V)): this.type = {
        map = map.updated(elem._1, elem._2)
        this
      }

      override def result(): SortedMap[K, V] = map

      override def clear(): Unit = map = empty
    }
  }


  implicit val stringOrder: Order[String] = new Order[String] {
    override def compare(x: String, y: String): Int = x compareTo y
  }
}
