package okasaki

import okasaki.RandomAccessList.Subscript
import okasaki.RedundantZerolessBinaryRandomAccessList._

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


  type SRList[E] = Stream[Digit[E]]
}

class RedundantZerolessBinaryRandomAccessList[E] extends RandomAccessList[E, SRList[E]] {
  override def empty: SRList[E] = Empty

  override def isEmpty: (SRList[E]) => Boolean = _.isEmpty

  def link(t1: Tree[E], t2: Tree[E]): Tree[E] = Node(t1.size + t2.size, t1, t2)

  // Potentially expensive operation 2
  def consTree(t: Tree[E], l: SRList[E]): SRList[E] = l match {
    case Empty => One(t) #:: Empty
    case One(t1) #:: ds => Two(t, t1) #:: ds
    case Two(t1, t2) #:: ds => Three(t, t1, t2) #:: ds
    case Three(t1, t2, t3) #:: ds => Two(t, t1) #:: consTree(link(t2, t3), ds)
  }

  override def cons: (SRList[E], E) => SRList[E] = {
    case (l, e) => consTree(Leaf(e), l)
  }

  override def head: (SRList[E]) => E = {
    case Empty => throw new IllegalArgumentException("head called on an empty list")
    case One(Leaf(x)) #:: _ => x
    case Two(Leaf(x), _) #:: _ => x
    case Three(Leaf(x), _, _) #:: _ => x
  }

  override def tail: (SRList[E]) => SRList[E] = ts => {

    // Expensive operation 1
    def unconsTree(l: SRList[E]): (Tree[E], SRList[E]) = l match {
      case Empty => throw new IllegalArgumentException("tail called on an empty list")
      case One(t) #:: Empty => (t, Empty)
      case Two(t1, t2) #:: ds => (t1, One(t2) #:: ds)
      case Three(t1, t2, t3) #:: ds => (t1, Two(t2, t3) #:: ds)

      case One(t) #:: One(Node(_, t1, t2)) #:: Empty => (t, Two(t1, t2) #:: Empty)
      case One(t) #:: Two(Node(_, t1, t2), t3) #:: ds => (t, Two(t1, t2) #:: One(t3) #:: ds)
      case One(t) #:: Three(Node(_, t1, t2), t3, t4) #:: ds => (t, Two(t1, t2) #:: Two(t3, t4) #:: ds)

      case One(t) #:: One(Node(_, t1, t2)) #:: ds =>
        (t, Two(t1, t2) #:: {
          val (Node(_, t11, t12), rest) = unconsTree(ds)
          Two(t11, t12) #:: rest
        })
    }

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
    case (_, Empty) => throw Subscript()
    case (i, One(t) #:: _) if i < t.size => lookupTree(i, t)
    case (i, One(t) #:: ts) => lookup(i - t.size, ts)
    case (i, Two(t1, _) #:: _) if i < t1.size => lookupTree(i, t1)
    case (i, Two(t1, t2) #:: _) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, Two(t1, _) #:: ts) => lookup(i - 2 * t1.size, ts)
    case (i, Three(t1, _, _) #:: _) if i < t1.size => lookupTree(i, t1)
    case (i, Three(t1, t2, _) #:: _) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, Three(t1, _, t3) #:: _) if i < 3 * t1.size => lookupTree(i - 2 * t1.size, t3)
    case (i, Three(t1, _, _) #:: ts) => lookup(i - 3 * t1.size, ts)
  }

  override def update: (Int, E, SRList[E]) => SRList[E] = {
    case (_, _, Empty) => throw Subscript()
    case (i, y, One(t) #:: ts) if i < t.size => One(updateTree(i, y, t)) #:: ts
    case (i, y, One(t) #:: ts) => One(t) #:: update(i - t.size, y, ts)
    case (i, y, Two(t1, t2) #:: ts) if i < t1.size => Two(updateTree(i, y, t1), t2) #:: ts
    case (i, y, Two(t1, t2) #:: ts) if i < 2 * t1.size => Two(t1, updateTree(i - t1.size, y, t2)) #:: ts
    case (i, y, Two(t1, t2) #:: ts) => Two(t1, t2) #:: update(i - 2 * t1.size, y, ts)
    case (i, y, Three(t1, t2, t3) #:: ts) if i < t1.size => Three(updateTree(i, y, t1), t2, t3) #:: ts
    case (i, y, Three(t1, t2, t3) #:: ts) if i < 2 * t1.size => Three(t1, updateTree(i - t1.size, y, t2), t3) #:: ts
    case (i, y, Three(t1, t2, t3) #:: ts) if i < 3 * t1.size => Three(t1, t2, updateTree(i - 2 * t1.size, y, t3)) #:: ts
    case (i, y, Three(t1, t2, t3) #:: ts) => Three(t1, t2, t3) #:: update(i - 3 * t1.size, y, ts)
  }
}
