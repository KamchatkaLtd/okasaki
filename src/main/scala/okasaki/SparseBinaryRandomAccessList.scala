package okasaki

import okasaki.RandomAccessList.Subscript
import okasaki.SparseBinaryRandomAccessList._

object SparseBinaryRandomAccessList {

  sealed trait Tree[E] {
    def size: Int
  }

  case class Leaf[E](x: E) extends Tree[E] {
    val size = 1
  }

  case class Node[E](size: Int, l: Tree[E], r: Tree[E]) extends Tree[E]

  type SRList[E] = List[Tree[E]]

  def complete[E](x: E, w: Int): Tree[E] =
    if (w == 1) Leaf(x)
    else {
      val tree = complete(x, w / 2)
      Node(w, tree, tree)
    }

  def repeat[E](n: Int, x: E): SRList[E] = {
    def repeat1(m: Int, w: Int): SRList[E] = {
      if (m == 0) Nil
      else {
        val tail = repeat1(m / 2, w * 2)
        if (m % 2 == 1) complete(x, w) :: tail
        else tail
      }
    }

    repeat1(n, 1)
  }
}

class SparseBinaryRandomAccessList[E] extends RandomAccessList[E, SRList[E]] {
  override def empty: SRList[E] = Nil

  override def isEmpty: (SRList[E]) => Boolean = _.isEmpty

  def link(t1: Tree[E], t2: Tree[E]): Tree[E] = Node(t1.size + t2.size, t1, t2)

  def consTree(t: Tree[E], l: SRList[E]): SRList[E] = l match {
    case Nil => List(t)
    case t2 :: _ if t.size < t2.size => t :: l
    case t2 :: ts if t.size == t2.size => consTree(link(t, t2), ts)
  }

  override def cons: (SRList[E], E) => SRList[E] = {
    case (l, e) => consTree(Leaf(e), l)
  }

  def unconsTree1(t: Tree[E], a: SRList[E]): (E, SRList[E]) = t match {
    case Leaf(x) => (x, a)
    case Node(_, t1, t2) => unconsTree1(t1, t2 :: a)
  }

  def unconsTree(l: SRList[E]): (E, SRList[E]) = l match {
    case Nil => throw new IllegalStateException("head or tail called on an empty list")
    case t :: ts => unconsTree1(t, ts)
  }

  override def head: (SRList[E]) => E = ts => {
    val (x, _) = unconsTree(ts)
    x
  }

  override def tail: (SRList[E]) => SRList[E] = ts => {
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

  override def lookup: (Int, SRList[E]) => E = {
    case (_, Nil) => throw Subscript()
    case (i, t :: _) if i < t.size => lookupTree(i, t)
    case (i, t :: ts) => lookup(i - t.size, ts)
  }

  override def update: (Int, E, SRList[E]) => SRList[E] = {
    case (_, _, Nil) => throw Subscript()
    case (i, y, t :: ts) if i < t.size => updateTree(i, y, t) :: ts
    case (i, y, t :: ts) => t :: update(i - t.size, y, ts)
  }

  def drop(n: Int, l: SRList[E]): SRList[E] = {
    def drop1(n: Int, l: SRList[E]): SRList[E] = (n, l) match {
      case (0, _) => l
      case (_, Nil) => throw Subscript()
      case (_, t :: ts) if n >= t.size => drop1(n - t.size, ts)
      case (_, t :: ts) => drop1(n - 1, unconsTree1(t, ts)._2)
    }
    drop1(n, l)
  }
}
