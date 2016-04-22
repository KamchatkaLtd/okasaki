package okasaki.maps

import okasaki.FiniteMap
import okasaki.FiniteMap.NotFound

import scala.util.Try

/**
 * Based on Trie implementation at Fig 10.10
 *
 * Copyright (C) 2016 Kamchatka Ltd
 */
class Trie[K, V](v: Option[V], m: FiniteMap[K, Trie[K, V]]) extends FiniteMap[List[K], V] {

  override def empty = new Trie[K, V](None, m.empty)

  @throws[NotFound[K]]
  override def lookup(k: List[K]): V = k match {
    case Nil => v getOrElse (throw new NotFound)
    case x :: xs => m.lookup(x) lookup xs
  }

  override def bind(k: List[K], v: V): Trie[K, V] = k match {
    case Nil =>
      new Trie(Some(v), m)
    case x :: xs =>
      val t = Try(m.lookup(x)) getOrElse empty
      val t1 = t.bind(xs, v)
      new Trie(this.v, m.bind(x, t1))
  }

  override def toString = s"Trie($m)"
}
