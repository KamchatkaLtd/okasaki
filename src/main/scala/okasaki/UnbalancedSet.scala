package okasaki

import scala.annotation.implicitNotFound

/**
 * Unbalanced set, based on the source at Fig. 2.9
 *
 * Includes ex. 2.2, 2.3, and 2.4
 *
 * Copyright (C) 2015 Kamchatka Ltd
 */
@implicitNotFound("No member of type class Ordered in scope for ${E}")
class UnbalancedSet[E](implicit ord: Ordered[E]) extends Set[E, Tree[E]] {
  import Tree._

  override def empty: Tree[E] = Empty

  override def member(x: E, s: Tree[E]): Boolean = {
    // ex. 2.2
    def member1(xx: E, last: Option[E], ss: Tree[E]): Boolean = {
      ss match {
        case Empty =>
          last.exists(y => ord.eq(xx, y))
        case SubTree(a, y, b) =>
          if (ord.lt(xx, y)) member1(xx, last, a)
          else member1(xx, Some(y), b)
      }
    }

    member1(x, None, s)
  }

  override def insert(x: E, s: Tree[E]): Tree[E] = {
    // ex. 2.3
    def insertTo(s: Tree[E]): Option[Tree[E]] = {
      s match {
        case Empty => Some(SubTree(Empty, x, Empty))
        case SubTree(a, y, b) =>
          if (ord.lt(x, y)) insertTo(a).map(SubTree(_, y, b))
          else if (ord.lt(y, x)) insertTo(b).map(SubTree(a, y, _))
          else None
      }
    }

    insertTo(s).getOrElse(s)
  }
}
