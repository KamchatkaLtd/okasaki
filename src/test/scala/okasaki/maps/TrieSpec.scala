package okasaki.maps

import okasaki.FiniteMap.NotFound
import okasaki.maps.TrieSpec.ScalaFiniteMap
import okasaki.{FiniteMap, FiniteMapSpec}
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object TrieSpec {

  class ScalaFiniteMap[K, V](m: Map[K, V]) extends FiniteMap[K, V] {
    override def empty =
      new ScalaFiniteMap[K, V](Map.empty)

    @throws[NotFound[K]]
    override def lookup(k: K): V =
      m.getOrElse(k, throw new NotFound(k))

    override def bind(k: K, v: V) =
      new ScalaFiniteMap[K, V](m + (k -> v))
  }

}

class TrieSpec extends FiniteMapSpec[List[Char], Int] {
  override implicit def keys: Arbitrary[List[Char]] = Arbitrary(Gen.listOf(Gen.alphaNumChar))

  override implicit def elements: Arbitrary[Int] = Arbitrary.arbInt

  override def map: FiniteMap[List[Char], Int] =
    new Trie[Char, Int](None, backingMap)

  val backingMap: FiniteMap[Char, Trie[Char, Int]] =
    new ScalaFiniteMap[Char, Trie[Char, Int]](Map.empty)

}
