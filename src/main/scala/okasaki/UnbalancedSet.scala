package okasaki

import scala.annotation.implicitNotFound

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
@implicitNotFound("No member of type class Ordered in scope for ${T}")
class UnbalancedSet[E](implicit ord: Ordered[E]) extends Set[E, Tree[E]] {
  override def empty: Tree[E] = Empty

  override def member(x: E, s: Tree[E]): Boolean = {
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

  override def insert(x: E, s: Tree[E]): Tree[E] = s match {
    case Empty => SubTree(Empty, x, Empty)
    case t@SubTree(a, y, b) =>
      if (ord.lt(x, y)) t.copy(left = insert(x, a))
      else if (ord.lt(y, x)) t.copy(right = insert(x, b))
      else t
  }
}
