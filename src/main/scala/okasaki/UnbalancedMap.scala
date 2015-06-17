package okasaki

import scala.annotation.implicitNotFound

/**
 * Unbalanced map, see ex. 2.6
 *
 * Copyright (C) 2015 Kamchatka Ltd
 */
@implicitNotFound("No member of type class Ordered in scope for ${K}, ${V}")
class UnbalancedMap[K, V](implicit ord: Ordered[K]) extends FiniteMap[K, V, Tree[(K, V)]] {
  override def empty: Tree[(K, V)] = Empty

  override def lookup(k: K, m: Tree[(K, V)]): V = {
    def lookup1(kk: K, last: Option[(K, V)], mm: Tree[(K, V)]): V = {
      mm match {
        case Empty =>
          last.filter(y => ord.eq(kk, y._1))
            .map(_._2).getOrElse(throw new NotFound())
        case SubTree(a, y, b) =>
          if (ord.lt(kk, y._1)) lookup1(kk, last, a)
          else lookup1(kk, Some(y), b)
      }
    }

    lookup1(k, None, m)
  }

  override def bind(k: K, v: V, s: Tree[(K, V)]): Tree[(K, V)] = {
    def bindIn(s: Tree[(K, V)]): Option[Tree[(K, V)]] = {
      s match {
        case Empty => Some(SubTree(Empty, (k, v), Empty))
        case SubTree(a, y, b) =>
          if (ord.lt(k, y._1)) bindIn(a).map(SubTree(_, y, b))
          else if (ord.lt(y._1, k)) bindIn(b).map(SubTree(a, y, _))
          else None
      }
    }

    bindIn(s).getOrElse(s)
  }
}
