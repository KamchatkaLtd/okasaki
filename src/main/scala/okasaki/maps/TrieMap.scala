package okasaki.maps

import okasaki.FiniteMap
import okasaki.FiniteMap.NotFound
import okasaki.maps.TrieMap.Trie

import scala.util.Try

/**
 * Based on Trie implementation at Fig 10.10
 *
 * Copyright (C) 2016 Kamchatka Ltd
 */
object TrieMap {

  case class Trie[K, V, M[_, _]](v: Option[V], m: M[K, Trie[K, V, M]])

}

trait TrieMap[K, V, M[_, _]] extends FiniteMap[List[K], V, Trie[K, V, M]] {

  def m: FiniteMap[K, Trie[K, V, M], M[K, Trie[K, V, M]]]

  override def empty = Trie[K, V, M](None, m.empty)

  @throws[NotFound[K]]
  override def lookup(k: List[K], t: Trie[K, V, M]): V = k match {
    case Nil => t.v getOrElse (throw new NotFound)
    case x :: xs => lookup(xs, m.lookup(x, t.m))
  }

  override def bind(k: List[K], v: V, t: Trie[K, V, M]): Trie[K, V, M] = k match {
    case Nil =>
      new Trie[K, V, M](Some(v), t.m)
    case x :: xs =>
      val t1 = Try(m.lookup(x, t.m)) getOrElse empty
      val t2 = bind(xs, v, t1)
      new Trie[K, V, M](t.v, m.bind(x, t2, t.m))
  }
}
