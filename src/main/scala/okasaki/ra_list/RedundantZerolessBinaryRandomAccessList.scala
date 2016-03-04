package okasaki.ra_list

import okasaki.RandomAccessList
import okasaki.RandomAccessList.Subscript
import okasaki.ra_list.RedundantZerolessBinaryRandomAccessList._

import scala.collection.immutable.Stream.Empty

object RedundantZerolessBinaryRandomAccessList {

  sealed trait Tree[E] {
    def size: Int
  }

  case class Leaf[E](x: E) extends Tree[E] {
    val size = 1
  }

  case class Node[E](size: Int, l: Tree[E], r: Tree[E]) extends Tree[E]


  sealed trait Digit[E]

  case class One[E](x: Tree[E]) extends Digit[E]

  case class Two[E](r: Tree[E], l: Tree[E]) extends Digit[E]

  case class Three[E](r: Tree[E], c: Tree[E], l: Tree[E]) extends Digit[E]


  type SRList[E] = (Stream[Digit[E]], Stream[Digit[E]])

  def exec[E](list: SRList[E]): SRList[E] = list match {
    case (xs, Empty) => list
    case (xs, _ #:: s) => (xs, s)
  }
}

class RedundantZerolessBinaryRandomAccessList[E] extends RandomAccessList[E, SRList[E]] {
  override def empty: SRList[E] = (Empty, Empty)

  override def isEmpty: (SRList[E]) => Boolean = _._1.isEmpty

  def link(t1: Tree[E], t2: Tree[E]): Tree[E] = Node(t1.size + t2.size, t1, t2)

  def consTree(t: Tree[E], l: Stream[Digit[E]]): (Stream[Digit[E]], Boolean) = l match {
    case Empty => (One(t) #:: Empty, false)
    case One(t1) #:: ds => (Two(t, t1) #:: ds, false)
    case Two(t1, t2) #:: ds => (Three(t, t1, t2) #:: ds, false)
    case Three(t1, t2, t3) #:: ds => (Two(t, t1) #:: consTree(link(t2, t3), ds)._1, true)
  }

  override def cons: (SRList[E], E) => SRList[E] = {
    case ((l, s), e) =>
      val (l1, reset) = consTree(Leaf(e), l)
      val result = if (reset) (l1, l1) else (l1, s)
      exec(result)
  }

  override def head: (SRList[E]) => E = {
    case (Empty, _) => throw new IllegalArgumentException("head called on an empty list")
    case (One(Leaf(x)) #:: _, _) => x
    case (Two(Leaf(x), _) #:: _, _) => x
    case (Three(Leaf(x), _, _) #:: _, _) => x
  }

  override def tail: (SRList[E]) => SRList[E] = ts => {

    def unconsTree(l: Stream[Digit[E]]): (Tree[E], Stream[Digit[E]], Boolean) = l match {
      case Empty => throw new IllegalArgumentException("tail called on an empty list")
      case One(t) #:: Empty => (t, Empty, false)
      case Two(t1, t2) #:: ds => (t1, One(t2) #:: ds, false)
      case Three(t1, t2, t3) #:: ds => (t1, Two(t2, t3) #:: ds, false)

      case One(t) #:: One(Node(_, t1, t2)) #:: Empty => (t, Two(t1, t2) #:: Empty, false)
      case One(t) #:: Two(Node(_, t1, t2), t3) #:: ds => (t, Two(t1, t2) #:: One(t3) #:: ds, false)
      case One(t) #:: Three(Node(_, t1, t2), t3, t4) #:: ds => (t, Two(t1, t2) #:: Two(t3, t4) #:: ds, false)

      case One(t) #:: One(Node(_, t1, t2)) #:: ds =>
        val ds1 = Two(t1, t2) #:: {
          val (Node(_, a, b), rest, _) = unconsTree(ds)
          Two(a, b) #:: rest
        }
        (t, ds1, true)
    }

    val (l, s) = ts
    val (_, ts1, reset) = unconsTree(l)
    val result = if (reset) (ts1, s) else (ts1, ts1)
    exec(result)
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
    case (_, (Empty, _)) => throw Subscript()
    case (i, (One(t) #:: _, _)) if i < t.size => lookupTree(i, t)
    case (i, (One(t) #:: ts, s)) => lookup(i - t.size, (ts, s))
    case (i, (Two(t1, _) #:: _, _)) if i < t1.size => lookupTree(i, t1)
    case (i, (Two(t1, t2) #:: _, _)) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, (Two(t1, _) #:: ts, s)) => lookup(i - 2 * t1.size, (ts, s))
    case (i, (Three(t1, _, _) #:: _, _)) if i < t1.size => lookupTree(i, t1)
    case (i, (Three(t1, t2, _) #:: _, _)) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, (Three(t1, _, t3) #:: _, _)) if i < 3 * t1.size => lookupTree(i - 2 * t1.size, t3)
    case (i, (Three(t1, _, _) #:: ts, s)) => lookup(i - 3 * t1.size, (ts, s))
  }

  override def update: (Int, E, SRList[E]) => SRList[E] = {
    case (_, _, (Empty, _)) => throw Subscript()
    case (i, y, (One(t) #:: ts, s)) if i < t.size => (One(updateTree(i, y, t)) #:: ts, s)
    case (i, y, (One(t) #:: ts, s)) => (One(t) #:: update(i - t.size, y, (ts, s))._1, s)
    case (i, y, (Two(t1, t2) #:: ts, s)) if i < t1.size => (Two(updateTree(i, y, t1), t2) #:: ts, s)
    case (i, y, (Two(t1, t2) #:: ts, s)) if i < 2 * t1.size => (Two(t1, updateTree(i - t1.size, y, t2)) #:: ts, s)
    case (i, y, (Two(t1, t2) #:: ts, s)) => (Two(t1, t2) #:: update(i - 2 * t1.size, y, (ts, Empty))._1, s)
    case (i, y, (Three(t1, t2, t3) #:: ts, s)) if i < t1.size => (Three(updateTree(i, y, t1), t2, t3) #:: ts, s)
    case (i, y, (Three(t1, t2, t3) #:: ts, s)) if i < 2 * t1.size => (Three(t1, updateTree(i - t1.size, y, t2), t3) #:: ts, s)
    case (i, y, (Three(t1, t2, t3) #:: ts, s)) if i < 3 * t1.size => (Three(t1, t2, updateTree(i - 2 * t1.size, y, t3)) #:: ts, s)
    case (i, y, (Three(t1, t2, t3) #:: ts, s)) => (Three(t1, t2, t3) #:: update(i - 3 * t1.size, y, (ts, Empty))._1, s)
  }
}
