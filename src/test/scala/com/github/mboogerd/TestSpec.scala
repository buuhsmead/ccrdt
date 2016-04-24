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
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.enablers.Containing
import org.scalatest.{FlatSpecLike, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.collection.JavaConversions._
/**
  *
  */
trait TestSpec extends FlatSpecLike with Matchers with GeneratorDrivenPropertyChecks {


  implicit def containingIterator[T]: Containing[java.lang.Iterable[T]] = new Containing[java.lang.Iterable[T]]{
    override def contains(container: java.lang.Iterable[T], element: Any): Boolean =
      container.toIterator.contains(element)

    override def containsOneOf(container: java.lang.Iterable[T], elements: Seq[Any]): Boolean =
      container.toSet.intersect(elements.toSet).size == 1 // <- Does not exit early once |intersection| > 1 is established

    override def containsNoneOf(container: java.lang.Iterable[T], elements: Seq[Any]): Boolean =
      container.toIterator.collectFirst{case elem if elements.contains(elem) => true }.isEmpty
  }
}
