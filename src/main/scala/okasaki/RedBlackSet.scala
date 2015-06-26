package okasaki

import okasaki.RedBlackSet.RBTree

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object RedBlackSet {

  sealed trait Color

  object R extends Color {
    override def toString = s"R"
  }

  object B extends Color {
    override def toString = s"B"
  }


  sealed trait RBTree[+E]

  object Empty extends RBTree[Nothing] {
    override def toString = s"E"
  }

  case class SubTree[E](c: Color, l: RBTree[E], e: E, r: RBTree[E]) extends RBTree[E] {
    override def toString = s"T($c,$l,$e,$r)"
  }

}

class RedBlackSet[E](implicit ord: Ordering[E]) extends Set[E, RBTree[E]] {

  import okasaki.RedBlackSet._

  override def empty: RBTree[E] = Empty

  override def member(x: E, s: RBTree[E]): Boolean = {
    def member1(last: Option[E], ss: RBTree[E]): Boolean = {
      ss match {
        case Empty =>
          last.exists(y => ord.equiv(x, y))
        case SubTree(_, a, y, b) =>
          if (ord.lt(x, y)) member1(last, a)
          else member1(Some(y), b)
      }
    }

    member1(None, s)
  }

  def balance(clr: Color, l: RBTree[E], e: E, r: RBTree[E]) = (clr, l, e, r) match {
    case (B, SubTree(R, SubTree(R, a, x, b), y, c), z, d) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case (B, SubTree(R, a, x, SubTree(R, b, y, c)), z, d) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case (B, a, x, SubTree(R, SubTree(R, b, y, c), z, d)) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case (B, a, x, SubTree(R, b, y, SubTree(R, c, z, d))) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case _ => SubTree(clr, l, e, r)
  }

  override def insert(x: E, s: RBTree[E]): RBTree[E] = {
    def ins(ss: RBTree[E]): SubTree[E] = ss match {
      case Empty => SubTree(R, Empty, x, Empty)
      case sss@SubTree(color, a, y, b) =>
        if (ord.lt(x, y)) balance(color, ins(a), y, b)
        else if (ord.lt(y, x)) balance(color, a, y, ins(b))
        else sss
    }

    ins(s).copy(c = B)
  }
}
