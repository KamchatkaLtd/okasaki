package okasaki.maps

import okasaki.FiniteMap
import okasaki.FiniteMap.NotFound
import okasaki.misc.BinaryTree.Empty
import okasaki.misc.{BinaryTree, SubBinaryTree}

import scala.annotation.implicitNotFound

/**
 * Unbalanced map, see ex. 2.6
 *
 * Copyright (C) 2015 Kamchatka Ltd
 */
@implicitNotFound("No member of type class Ordering in scope for ${K}")
class UnbalancedMap[K, V](m: BinaryTree[(K, V)] = Empty)
                         (implicit ord: Ordering[K])

  extends FiniteMap[K, V] {

  import okasaki.misc.BinaryTree._

  override def empty = new UnbalancedMap[K, V]()

  override def lookup(k: K): V = {
    def lookup1(kk: K, last: Option[(K, V)], mm: BinaryTree[(K, V)]): V = {
      mm match {
        case Empty =>
          last.filter(y => ord.eq(kk, y._1))
            .map(_._2).getOrElse(throw new NotFound())
        case SubBinaryTree(a, y, b) =>
          if (ord.lt(kk, y._1)) lookup1(kk, last, a)
          else if (ord.gt(kk, y._1)) lookup1(kk, Some(y), b)
          else y._2
      }
    }

    lookup1(k, None, m)
  }

  override def bind(k: K, v: V) = {
    def bindIn(s: BinaryTree[(K, V)]): Option[BinaryTree[(K, V)]] = {
      s match {
        case Empty => Some(SubBinaryTree(Empty, (k, v), Empty))
        case SubBinaryTree(a, y, b) if y == (k, v) => None
        case SubBinaryTree(a, y, b) =>
          if (ord.lt(k, y._1)) bindIn(a).map(SubBinaryTree(_, y, b))
          else if (ord.lt(y._1, k)) bindIn(b).map(SubBinaryTree(a, y, _))
          else Some(SubBinaryTree(a, (k, v), b))
      }
    }

    bindIn(m) map (new UnbalancedMap[K, V](_)) getOrElse this
  }

  override def toString = s"UnbalancedMap($m)"
}
