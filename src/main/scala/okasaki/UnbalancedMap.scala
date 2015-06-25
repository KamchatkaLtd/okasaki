package okasaki

import scala.annotation.implicitNotFound

/**
 * Unbalanced map, see ex. 2.6
 *
 * Copyright (C) 2015 Kamchatka Ltd
 */
@implicitNotFound("No member of type class Ordering in scope for ${K}, ${V}")
class UnbalancedMap[K, V](implicit ord: Ordering[K]) extends FiniteMap[K, V, BinaryTree[(K, V)]] {
  import BinaryTree._

  override def empty: BinaryTree[(K, V)] = Empty

  override def lookup(k: K, m: BinaryTree[(K, V)]): V = {
    def lookup1(kk: K, last: Option[(K, V)], mm: BinaryTree[(K, V)]): V = {
      mm match {
        case Empty =>
          last.filter(y => ord.eq(kk, y._1))
            .map(_._2).getOrElse(throw new NotFound())
        case SubBinaryTree(a, y, b) =>
          if (ord.lt(kk, y._1)) lookup1(kk, last, a)
          else lookup1(kk, Some(y), b)
      }
    }

    lookup1(k, None, m)
  }

  override def bind(k: K, v: V, s: BinaryTree[(K, V)]): BinaryTree[(K, V)] = {
    def bindIn(s: BinaryTree[(K, V)]): Option[BinaryTree[(K, V)]] = {
      s match {
        case Empty => Some(SubBinaryTree(Empty, (k, v), Empty))
        case SubBinaryTree(a, y, b) =>
          if (ord.lt(k, y._1)) bindIn(a).map(SubBinaryTree(_, y, b))
          else if (ord.lt(y._1, k)) bindIn(b).map(SubBinaryTree(a, y, _))
          else None
      }
    }

    bindIn(s).getOrElse(s)
  }
}
