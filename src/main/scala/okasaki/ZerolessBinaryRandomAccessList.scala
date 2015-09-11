package okasaki

import okasaki.RandomAccessList.Subscript
import okasaki.ZerolessBinaryRandomAccessList._


object ZerolessBinaryRandomAccessList {

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


  type SRList[E] = List[Digit[E]]
}

class ZerolessBinaryRandomAccessList[E] extends RandomAccessList[E, SRList[E]] {
  override def empty: SRList[E] = Nil

  override def isEmpty: (SRList[E]) => Boolean = _.isEmpty

  def link(t1: Tree[E], t2: Tree[E]): Tree[E] = Node(t1.size + t2.size, t1, t2)

  def consTree(t: Tree[E], l: SRList[E]): SRList[E] = l match {
    case Nil => List(One(t))
    case One(t1) :: ds => Two(t, t1) :: ds
    case Two(t1, t2) :: ds => One(t) :: consTree(link(t1, t2), ds)
  }

  override def cons: (SRList[E], E) => SRList[E] = {
    case (l, e) => consTree(Leaf(e), l)
  }

  override def head: (SRList[E]) => E = {
    case Nil => throw new IllegalArgumentException("head called on an empty list")
    case One(Leaf(x)) :: _ => x
    case Two(Leaf(x), _) :: _ => x
  }

  override def tail: (SRList[E]) => SRList[E] = ts => {

    def unconsTree(l: SRList[E]): (Tree[E], SRList[E]) = l match {
      case Nil => throw new IllegalArgumentException("tail called on an empty list")
      case One(t) :: Nil => (t, Nil)
      case Two(t1, t2) :: ds => (t1, One(t2) :: ds)
      case One(t) :: ds =>
        val (Node(_, t1, t2), rest) = unconsTree(ds)
        (t, Two(t1, t2) :: rest)
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
    case (_, Nil) => throw Subscript()
    case (i, One(t) :: _) if i < t.size => lookupTree(i, t)
    case (i, One(t) :: ts) => lookup(i - t.size, ts)
    case (i, Two(t1, _) :: _) if i < t1.size => lookupTree(i, t1)
    case (i, Two(t1, t2) :: _) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, Two(t1, _) :: ts) => lookup(i - 2 * t1.size, ts)
  }

  override def update: (Int, E, SRList[E]) => SRList[E] = {
    case (_, _, Nil) => throw Subscript()
    case (i, y, One(t) :: ts) if i < t.size => One(updateTree(i, y, t)) :: ts
    case (i, y, One(t) :: ts) => One(t) :: update(i - t.size, y, ts)
    case (i, y, Two(t1, t2) :: ts) if i < t1.size => Two(updateTree(i, y, t1), t2) :: ts
    case (i, y, Two(t1, t2) :: ts) if i < 2 * t1.size => Two(t1, updateTree(i - t1.size, y, t2)) :: ts
    case (i, y, Two(t1, t2) :: ts) => Two(t1, t2) :: update(i - 2 * t1.size, y, ts)
  }

  def expand(l: SRList[E], n: Int): SRList[E] = l match {
    case Nil => Nil
    case One(Leaf(t)) :: ts => One(Leaf(t)) :: expand(ts, 2)
    case Two(Leaf(t1), Leaf(t2)) :: ts => Two(Leaf(t1), Leaf(t2)) :: expand(ts, 2)
    case One(Node(`n`, t1, t2)) :: ts => One(Node(`n`, t1, t2)) :: expand(ts, n * 2)
    case Two(Node(`n`, t1, t2), t3) :: ts => Two(Node(`n`, t1, t2), t3) :: expand(ts, n * 2)
    case One(Node(_, t1, t2)) :: ts => expand(Two(t1, t2) :: ts, n)
    case Two(Node(_, t1, t2), t3) :: ts => expand(Two(t1, t2) :: One(t3) :: ts, n)
  }

  def drop(n: Int, l: SRList[E]): SRList[E] = {
    def drop1(n: Int, l: SRList[E]): SRList[E] = (n, l) match {
      case (0, _) => l
      case (_, Nil) => throw Subscript()
      case (_, One(t) :: ts) if n >= t.size => drop1(n - t.size, ts)
      case (_, Two(t1, _) :: ts) if n >= t1.size * 2 => drop1(n - 2 * t1.size, ts)
      case (_, Two(t1, t2) :: ts) if n >= t1.size => drop1(n - t1.size, One(t2) :: ts)
      case _ => drop1(n, expand(l, 1))
    }
    val list = drop1(n, l)
    expand(list, 1)
  }
}
