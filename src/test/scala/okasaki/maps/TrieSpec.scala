package okasaki.maps

import okasaki.FiniteMap.NotFound
import okasaki.maps.TrieMap.Trie
import okasaki.maps.TrieSpec._
import okasaki.{FiniteMap, FiniteMapSpec}
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object TrieSpec {

  class ScalaFiniteMap[K, V] extends FiniteMap[K, V, Map[K, V]] {
    override def empty = Map.empty

    @throws[NotFound[K]]
    override def lookup(k: K, m: Map[K, V]): V = m.getOrElse(k, throw new NotFound(k))

    override def bind(k: K, v: V, m: Map[K, V]) = m + (k -> v)
  }
}

class TrieSpec extends FiniteMapSpec[List[Char], Int, Trie[Char, Int, Map]] {
  override implicit def keys: Arbitrary[List[Char]] = Arbitrary(Gen.listOf(Gen.alphaNumChar))

  override implicit def elements: Arbitrary[Int] = Arbitrary.arbInt

  override def map: FiniteMap[List[Char], Int, Trie[Char, Int, Map]] =
    new TrieMap[Char, Int, Map] {
      override def m = new ScalaFiniteMap[Char, Trie[Char, Int, Map]]()
    }
}
