package okasaki.sets

import java.lang.Math._

import okasaki.Set
import okasaki.misc.Aux.log2
import okasaki.sets.RedBlackSet._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object RedBlackSet {

  sealed trait Color

  object R extends Color {
    override def toString = "R"
  }

  object B extends Color {
    override def toString = "B"
  }


  sealed trait RBTree[+E]

  object Empty extends RBTree[Nothing] {
    override def toString = "E"
  }

  case class SubTree[E](c: Color, l: RBTree[E], e: E, r: RBTree[E]) extends RBTree[E] {
    override def toString = s"T($c,$l,$e,$r)"
  }

  // ex. 3.9
  def fromOrdList[E](l: List[E])(implicit ord: Ordering[E]): RedBlackSet[E] = {
    def build(l: List[E], rb: Int): RBTree[E] = (l, rb) match {
      case (Nil, 0) => Empty
      case (Nil, _) => throw new IllegalStateException(s"Red budget of $rb allocated for an empty list")
      case (x :: Nil, 0) => SubTree(B, Empty, x, Empty)
      case (x :: Nil, 1) => SubTree(R, Empty, x, Empty)
      case (x :: Nil, _) => throw new IllegalStateException(s"Red budget of $rb allocated for a singleton list")
      case (x :: y :: Nil, 1) => SubTree(B, SubTree(R, Empty, x, Empty), y, Empty)
      case (x :: y :: Nil, _) => throw new IllegalStateException(s"Red budget of $rb allocated for a list of size 2")
      case (x :: y :: z :: Nil, 0) => SubTree(B, SubTree(B, Empty, x, Empty), y, SubTree(B, Empty, z, Empty))
      case (x :: y :: z :: Nil, 2) => SubTree(B, SubTree(R, Empty, x, Empty), y, SubTree(R, Empty, z, Empty))
      case (x :: y :: z :: Nil, _) => throw new IllegalStateException(s"Red budget of $rb allocated for a list of size 3")
      case _ =>
        val halves = l.splitAt(l.size / 2)
        SubTree(B, build(halves._1, (rb + 1) / 2), halves._2.head, build(halves._2.tail, rb / 2))
    }

    val redBudget = l.size - nearestFullTreeSmallerThan(l.size)
    new RedBlackSet(build(l, redBudget))
  }

  def nearestFullTreeSmallerThan(n: Int): Int = (1 << log2(n + 1)) - 1
}

class RedBlackSet[E](s: RBTree[E] = Empty)(implicit ord: Ordering[E]) extends Set[E] {

  override def empty = new RedBlackSet[E]()

  override def member(x: E): Boolean = {
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

  def blackHeight(s: RBTree[_]): Int = s match {
    case Empty => 0
    case SubTree(R, a, _, b) => max(blackHeight(a), blackHeight(b))
    case SubTree(B, a, _, b) => 1 + max(blackHeight(a), blackHeight(b))
  }

  def isValid: Boolean = isValid(s)

  def isValid(s: RBTree[_]): Boolean = s match {
    case Empty => true
    case SubTree(_, a, _, b) => blackHeight(a) == blackHeight(b) && isValid(a) && isValid(b)
  }

  def lbalance(clr: Color, l: RBTree[E], e: E, r: RBTree[E]) = (clr, l, e, r) match {
    case (B, SubTree(R, SubTree(R, a, x, b), y, c), z, d) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case (B, SubTree(R, a, x, SubTree(R, b, y, c)), z, d) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case _ => SubTree(clr, l, e, r)
  }

  def rbalance(clr: Color, l: RBTree[E], e: E, r: RBTree[E]) = (clr, l, e, r) match {
    case (B, a, x, SubTree(R, SubTree(R, b, y, c), z, d)) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case (B, a, x, SubTree(R, b, y, SubTree(R, c, z, d))) => SubTree(R, SubTree(B, a, x, b), y, SubTree(B, c, z, d))
    case _ => SubTree(clr, l, e, r)
  }

  override def insert(x: E) = {
    def ins(ss: RBTree[E]): SubTree[E] = ss match {
      case Empty => SubTree(R, Empty, x, Empty)
      case sss@SubTree(color, a, y, b) =>
        if (ord.lt(x, y)) lbalance(color, ins(a), y, b)
        else if (ord.lt(y, x)) rbalance(color, a, y, ins(b))
        else sss
    }

    new RedBlackSet(ins(s).copy(c = B))
  }
}
