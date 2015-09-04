package okasaki

import okasaki.BinaryRandomAccessList._
import okasaki.RandomAccessList.Subscript

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

object BinaryRandomAccessList {

  sealed trait Tree[E] {
    def size: Int
  }

  case class Leaf[E](x: E) extends Tree[E] {
    val size = 1
  }

  case class Node[E](size: Int, l: Tree[E], r: Tree[E]) extends Tree[E]


  sealed trait Digit[+E]

  object Zero extends Digit[Nothing] {
    override def toString = "Zero"
  }

  case class One[E](e: Tree[E]) extends Digit[E]


  type RList[E] = List[Digit[E]]
}

class BinaryRandomAccessList[E] extends RandomAccessList[E, RList[E]] {
  override def empty: RList[E] = Nil

  override def isEmpty: (RList[E]) => Boolean = _.isEmpty

  def link(t1: Tree[E], t2: Tree[E]): Tree[E] = Node(t1.size + t2.size, t1, t2)

  def consTree(t: Tree[E], l: RList[E]): RList[E] = l match {
    case Nil => List(One(t))
    case Zero :: l1 => One(t) :: l1
    case One(t2) :: l1 => Zero :: consTree(link(t, t2), l1)
  }

  override def cons: (RList[E], E) => RList[E] = {
    case (l, e) => consTree(Leaf(e), l)
  }

  def unconsTree(l: RList[E]): (Tree[E], RList[E]) = l match {
    case Nil => throw new IllegalStateException("head or tail called on an empty list")
    case One(t) :: Nil => (t, Nil)
    case One(t) :: l1 => (t, Zero :: l1)
    case Zero :: ts =>
      val (Node(_, t1, t2), l2) = unconsTree(ts)
      (t1, One(t2) :: l2)
  }

  override def head: (RList[E]) => E = ts => {
    val (Leaf(x), _) = unconsTree(ts)
    x
  }

  override def tail: (RList[E]) => RList[E] = ts => {
    val (_, ts1) = unconsTree(ts)
    ts1
  }

  def lookupTree(i: Int, t: Tree[E]): E = (i, t) match {
    case (0, Leaf(x)) => x
    case (_, Leaf(_)) => throw Subscript()
    case (_, Node(w, t1, t2)) =>
      val halfSize = w / 2
      if (i < halfSize) lookupTree(i, t1)
      else lookupTree(i - halfSize, t2)
  }

  def updateTree(i: Int, y: E, t: Tree[E]): Tree[E] = (i, t) match {
    case (0, Leaf(_)) => Leaf(y)
    case (_, Leaf(_)) => throw Subscript()
    case (_, Node(w, t1, t2)) =>
      val halfSize = w / 2
      if (i < halfSize) Node(w, updateTree(i, y, t1), t2)
      else Node(w, t1, updateTree(i - halfSize, y, t2))
  }

  override def lookup: (Int, RList[E]) => E = {
    case (_, Nil) => throw Subscript()
    case (i, Zero :: ts) => lookup(i, ts)
    case (i, One(t) :: _) if i < t.size => lookupTree(i, t)
    case (i, One(t) :: ts) => lookup(i - t.size, ts)
  }

  override def update: (Int, E, RList[E]) => RList[E] = {
    case (_, _, Nil) => throw Subscript()
    case (i, y, Zero :: ts) => Zero :: update(i, y, ts)
    case (i, y, One(t) :: ts) if i < t.size => One(updateTree(i, y, t)) :: ts
    case (i, y, One(t) :: ts) => One(t) :: update(i - t.size, y, ts)
  }

  def expand(l: RList[E]): RList[E] = {
    def normalize(n: Int, ll: RList[E]): RList[E] = ll match {
      case Nil => Nil
      case One(t) :: ts if t.size == n => One(t) :: normalize(n * 2, ts)
      case ts@(One(_) :: _) => Zero :: normalize(n * 2, ts)
      case Zero :: ts => Zero :: normalize(n * 2, ts)
    }
    normalize(1, l)
  }

  def minusOne(t: Tree[E], a: RList[E]): RList[E] = t match {
    case Leaf(_) => a
    case Node(_, t1, t2) => minusOne(t1, One(t2) :: a)
  }

  def drop(n: Int, l: RList[E]): RList[E] = {
    def drop1(n: Int, l: RList[E]): RList[E] = (n, l) match {
      case (0, _) => l
      case (_, Nil) => throw Subscript()
      case (_, Zero :: ts) => drop1(n, ts)
      case (_, One(t) :: ts) if n > t.size => drop1(n - t.size, ts)
      case (_, One(t) :: ts) if n == t.size => ts
      case (_, One(t) :: Nil) => drop1(n - 1, minusOne(t, Nil))
      case (_, One(t) :: ts) => drop1(n - 1, minusOne(t, Zero :: ts))
    }
    val ll: RList[E] = drop1(n, l)
    expand(ll)
  }
}
