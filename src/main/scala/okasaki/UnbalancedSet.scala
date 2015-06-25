package okasaki

import scala.annotation.implicitNotFound

/**
 * Unbalanced set, based on the source at Fig. 2.9
 *
 * Includes ex. 2.2, 2.3, and 2.4
 *
 * Copyright (C) 2015 Kamchatka Ltd
 */
@implicitNotFound("No member of type class Ordering in scope for ${E}")
class UnbalancedSet[E](implicit ord: Ordering[E]) extends Set[E, BinaryTree[E]] {
  import BinaryTree._

  override def empty: BinaryTree[E] = Empty

  override def member(x: E, s: BinaryTree[E]): Boolean = {
    // ex. 2.2
    def member1(last: Option[E], ss: BinaryTree[E]): Boolean = {
      println(s"member1($last, $ss)")
      ss match {
        case Empty =>
          last.exists(y => ord.equiv(x, y))
        case SubBinaryTree(a, y, b) =>
          if (ord.lt(x, y)) member1(last, a)
          else member1(Some(y), b)
      }
    }

    member1(None, s)
  }

  override def insert(x: E, s: BinaryTree[E]): BinaryTree[E] = {
    // ex. 2.3
    def insertTo(s: BinaryTree[E]): Option[BinaryTree[E]] = {
      s match {
        case Empty => Some(SubBinaryTree(Empty, x, Empty))
        case SubBinaryTree(a, y, b) =>
          if (ord.lt(x, y)) insertTo(a).map(SubBinaryTree(_, y, b))
          else if (ord.lt(y, x)) insertTo(b).map(SubBinaryTree(a, y, _))
          else None
      }
    }

    insertTo(s).getOrElse(s)
  }
}
