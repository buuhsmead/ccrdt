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

package com.github.mboogerd.experiments

import java.io.{ByteArrayOutputStream, ObjectOutputStream, Serializable}
import java.util.Base64

import com.github.mboogerd.TestSpec
import com.twitter.chill._
import org.scalacheck.Arbitrary._

/**
  *
  */
object ChillTest {

  protected def kryo: KryoPool = ScalaKryoInstantiator.defaultPool

  private def serializableToString(o: Serializable): String = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject( o )
    oos.close()
    toBase64(baos.toByteArray)
  }

  def toBase64(baos: Array[Byte]): String = Base64.getEncoder.encodeToString(baos)
}
class ChillTest extends TestSpec {

  import ChillTest._

  val f: (Int) => (Int) = _ + 1

  "Chill" should "be able to use the meatlocker to serialize a function" in {
    val meatLockerF = MeatLocker(f)
    val serializedFunction = serializableToString(meatLockerF)
    serializedFunction should not have length(0)
  }

  "KryoPool" should "allow serializing a function" in {
    val output = new Output(256, -1)
    val bytes = kryo.toBytesWithClass(f)
    println("Encoded function1: " + toBase64(bytes))
    bytes should not have length(0)
  }

  it should "deserialize correct binary data to a function instance" in {
    val bytes = kryo.toBytesWithClass(f)
    val deserializedFunction = kryo.fromBytes(bytes).asInstanceOf[Function1[Int, Int]]
    deserializedFunction should not be null
  }

  it should "not influence the result of applying the function before and after serialization" in {
    val bytes = kryo.toBytesWithClass(f)
    val deserializedFunction = kryo.fromBytes(bytes).asInstanceOf[Function1[Int, Int]]

    forAll(arbitrary[Int]) { i =>
      deserializedFunction(i) should equal (f(i))
    }
  }

}
