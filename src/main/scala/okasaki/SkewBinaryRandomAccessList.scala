package okasaki

import okasaki.SkewBinaryRandomAccessList.{Leaf, Node, SkewList, Tree}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object SkewBinaryRandomAccessList {

  sealed trait Tree[E] {
    def x: E
  }

  case class Leaf[E](x: E) extends Tree[E]

  case class Node[E](x: E, r: Tree[E], l: Tree[E]) extends Tree[E]

  type SkewList[E] = List[(Int, Tree[E])]
}

class SkewBinaryRandomAccessList[E] extends RandomAccessList[E, SkewList[E]] {
  override def empty: SkewList[E] = Nil

  override def isEmpty: (SkewList[E]) => Boolean = _.isEmpty

  override def cons: (SkewList[E], E) => SkewList[E] = {
    case (ts@((w1, t1) :: (w2, t2) :: rest), x) =>
      if (w1 == w2) (1 + w1 + w2, Node(x, t1, t2)) :: rest
      else (1, Leaf(x)) :: ts
    case (ts, x) =>
      (1, Leaf(x)) :: ts
  }

  override def head: (SkewList[E]) => E = {
    case (1, Leaf(x)) :: ts => x
    case (_, Node(x, _, _)) :: ts => x
  }

  override def tail: (SkewList[E]) => SkewList[E] = {
    case (1, Leaf(_)) :: ts => ts
    case (w, Node(_, t1, t2)) :: ts => (w / 2, t1) ::(w / 2, t2) :: ts
  }

  def lookupTree(w: Int, i: Int, t: Tree[E]): E = (w, i, t) match {
    case (1, 0, Leaf(x)) => x
    case (_, 0, Node(x, _, _)) => x
    case (_, _, Node(_, t1, t2)) =>
      if (i <= w / 2) lookupTree(w / 2, i - 1, t1)
      else lookupTree(w / 2, i - 1 - w / 2, t2)
  }

  override def lookup: (Int, SkewList[E]) => E = {
    case (i, (w, t) :: ts) =>
      if (i < w) lookupTree(w, i, t)
      else lookup(i - w, ts)
  }

  def updateTree(w: Int, i: Int, y: E, t: Tree[E]): Tree[E] = (w, i, t) match {
    case (1, 0, Leaf(_)) => Leaf(y)
    case (_, 0, Node(_, t1, t2)) => Node(y, t1, t2)
    case (_, _, Node(x, t1, t2)) =>
      if (i <= w / 2) Node(x, updateTree(w / 2, i - 1, y, t1), t2)
      else Node(x, t1, updateTree(w / 2, i - 1 - w / 2, y, t2))
  }

  override def update: (Int, E, SkewList[E]) => SkewList[E] = {
    case (i, y, (w, t) :: ts) =>
      if (i < w) (w, updateTree(w, i, y, t)) :: ts
      else (w, t) :: update(i - w, y, ts)
  }
}
