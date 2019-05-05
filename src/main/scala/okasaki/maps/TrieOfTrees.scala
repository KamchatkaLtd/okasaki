package okasaki.maps

import okasaki.FiniteMap
import okasaki.FiniteMap.NotFound
import okasaki.maps.TrieOfTrees._

import scala.language.higherKinds
import scala.util.Try

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object TrieOfTrees {

  sealed trait Tree[+M]

  case object E extends Tree[Nothing]

  case class T[M](x: M, l: Tree[M], r: Tree[M]) extends Tree[M]

  case class Trie[K, V, M[_, _]](v: Option[V], m: M[K, Trie[K, Trie[K, V, M], M]])

  trait FiniteMapLike[M[_, _]] {
    def ops[K, V]: FiniteMap[K, V, M[K, V]]
  }

  def empty[K, V, M[_, _] : FiniteMapLike]: Trie[K, V, M] = {
    val map = implicitly[FiniteMapLike[M]].ops[K, Trie[K, Trie[K, V, M], M]]
    Trie[K, V, M](None, map.empty)
  }

  //@throws[NotFound[K]]
  def lookup[K, V, M[_, _] : FiniteMapLike](k: Tree[K], t: Trie[K, V, M]): V = {
    val map = implicitly[FiniteMapLike[M]].ops[K, Trie[K, Trie[K, V, M], M]]
    (k, t) match {
      case (E, Trie(None, _)) => throw new NotFound(k)
      case (E, Trie(Some(x), _)) => x
      case (T(x, l, r), Trie(_, m)) => lookup(r, lookup(l, map.lookup(x, m)))
    }
  }

  def bind[K, V, M[_, _] : FiniteMapLike](k: Tree[K], v: V, t: Trie[K, V, M]): Trie[K, V, M] = {
    val map = implicitly[FiniteMapLike[M]].ops[K, Trie[K, Trie[K, V, M], M]]
    k match {
      case E => t.copy(v = Some(v))
      case T(x, l, r) =>
        val tt = Try(map.lookup(x, t.m)) getOrElse empty[K, Trie[K, V, M], M]
        val t1 = Try(lookup(l, tt)) getOrElse empty[K, V, M]
        val t2 = bind(r, v, t1)
        val tt1 = bind(l, t2, tt)
        Trie[K, V, M](t.v, map.bind(x, tt1, t.m))
    }
  }
}

trait TrieOfTrees[K, V, M[_, _]] extends FiniteMap[Tree[K], V, Trie[K, V, M]] {

  implicit def map: FiniteMapLike[M]

  override def empty: Trie[K, V, M] = TrieOfTrees.empty[K, V, M]

  @throws[NotFound[K]]
  override def lookup(k: Tree[K], m: Trie[K, V, M]): V = TrieOfTrees.lookup(k, m)

  override def bind(k: Tree[K], v: V, m: Trie[K, V, M]): Trie[K, V, M] = TrieOfTrees.bind(k, v, m)
}
