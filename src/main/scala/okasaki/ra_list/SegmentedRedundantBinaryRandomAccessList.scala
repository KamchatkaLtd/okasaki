package okasaki.ra_list

import okasaki.RandomAccessList
import okasaki.RandomAccessList.Subscript
import okasaki.ra_list.SegmentedRedundantBinaryRandomAccessList._

object SegmentedRedundantBinaryRandomAccessList {

  sealed trait Tree[E] {
    def size: Int
  }

  case class Leaf[E](x: E) extends Tree[E] {
    val size = 1
  }

  case class Node[E](size: Int, l: Tree[E], r: Tree[E]) extends Tree[E]


  sealed trait Digit[+E]

  case object Zero extends Digit[Nothing]

  case class Ones[E](ts: List[Tree[E]]) extends Digit[E]

  case class Two[E](a: Tree[E], b: Tree[E]) extends Digit[E]

  case class Threes[E](ts: List[(Tree[E], Tree[E], Tree[E])]) extends Digit[E]

  case class Four[E](a: Tree[E], b: Tree[E], c: Tree[E], d: Tree[E]) extends Digit[E]


  type SRList[E] = List[Digit[E]]
}

class SegmentedRedundantBinaryRandomAccessList[E] extends RandomAccessList[E, SRList[E]] {
  override def empty: SRList[E] = Nil

  override def isEmpty: (SRList[E]) => Boolean = _.isEmpty

  def link(t1: Tree[E], t2: Tree[E]): Tree[E] = Node(t1.size + t2.size, t1, t2)

  def ones(n: List[(Tree[E])], ds: SRList[E]): SRList[E] = (n, ds) match {
    case (Nil, _) => ds
    case (_, Ones(m) :: ds1) => Ones(n ++ m) :: ds1
    case _ => Ones(n) :: ds
  }

  def threes(n: List[(Tree[E], Tree[E], Tree[E])], ds: SRList[E]): SRList[E] = (n, ds) match {
    case (Nil, _) => ds
    case (_, Threes(m) :: ds1) => Threes(n ++ m) :: ds1
    case _ => Threes(n) :: ds
  }

  def fixup(x: SRList[E]): SRList[E] = x match {
    case Zero :: Nil => Nil
    case Zero :: ds =>
      val (Node(_, a, b), rest) = simpleDec(ds)
      Two(a, b) :: rest
    case Ones(n) :: Zero :: ds =>
      val (Node(_, a, b), rest) = simpleDec(ds)
      Ones(n) :: Two(a, b) :: rest
    case Threes(n) :: Four(a, b, c, d) :: ds =>
      Threes(n) :: Two(a, b) :: simpleInc(link(c, d), ds)
    case Four(a, b, c, d) :: ds =>
      Two(a, b) :: simpleInc(link(c, d), ds)
    case _ => x
  }

  def simpleInc(t: Tree[E], x: SRList[E]): SRList[E] = x match {
    case Nil => ones(List(t), Nil)
    case Zero :: ds => ones(List(t), ds)
    case Ones(a :: ts) :: ds => Two(t, a) :: ones(ts, ds)
    case Two(a, b) :: ds => threes(List((t, a, b)), ds)
    case Threes((a, b, c) :: ts) :: ds => Four(t, a, b, c) :: threes(ts, ds)
    case Four(a, b, c, d) :: ds => Two(a, b) :: simpleInc(link(c, d), ds)
  }

  def simpleDec(x: SRList[E]): (Tree[E], SRList[E]) = x match {
    case Nil => throw new IllegalArgumentException("Cannot decrement zero")
    case Zero :: ds =>
      val (Node(_, a, b), rest) = simpleDec(ds)
      (a, ones(List(b), rest))
    case Ones(t :: Nil) :: Nil =>
      (t, Nil)
    case Ones(t :: ts) :: ds =>
      (t, Zero :: ones(ts, ds))
    case Two(a, b) :: ds => (a, ones(List(b), ds))
    case Threes((a, b, c) :: ts) :: ds => (a, Two(b, c) :: threes(ts, ds))
    case Four(a, b, c, d) :: ds => (a, threes(List((b, c, d)), ds))
  }

  def consTree(t: Tree[E], l: SRList[E]): SRList[E] = fixup(simpleInc(t, l))

  def unconsTree(l: SRList[E]): (Tree[E], SRList[E]) = {
    val (t, rest) = simpleDec(l)
    (t, fixup(rest))
  }

  override def cons: (SRList[E], E) => SRList[E] = {
    case (l, e) => consTree(Leaf(e), l)
  }

  override def head: (SRList[E]) => E = {
    case Nil => throw new IllegalArgumentException("head called on an empty list")
    case l =>
      val (Leaf(x), _) = unconsTree(l)
      x
  }

  override def tail: (SRList[E]) => SRList[E] = ts => unconsTree(ts)._2

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
    case (i, Zero :: ds) => lookup(i, ds)
    case (i, Ones(t :: _) :: _) if i < t.size => lookupTree(i, t)
    case (i, Ones(t :: ts) :: ds) => lookup(i - t.size, ones(ts, ds))
    case (i, Two(t1, _) :: _) if i < t1.size => lookupTree(i, t1)
    case (i, Two(t1, t2) :: _) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, Two(t1, _) :: ts) => lookup(i - 2 * t1.size, ts)
    case (i, Threes((t1, _, _) :: _) :: _) if i < t1.size => lookupTree(i, t1)
    case (i, Threes((t1, t2, _) :: _) :: _) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, Threes((t1, _, t3) :: _) :: _) if i < 3 * t1.size => lookupTree(i - 2 * t1.size, t3)
    case (i, Threes((t1, _, _) :: ts) :: ds) => lookup(i - 3 * t1.size, threes(ts, ds))
    case (i, Four(t1, _, _, _) :: _) if i < t1.size => lookupTree(i, t1)
    case (i, Four(t1, t2, _, _) :: _) if i < 2 * t1.size => lookupTree(i - t1.size, t2)
    case (i, Four(t1, _, t3, _) :: _) if i < 3 * t1.size => lookupTree(i - 2 * t1.size, t3)
    case (i, Four(t1, _, _, t4) :: _) if i < 4 * t1.size => lookupTree(i - 3 * t1.size, t4)
    case (i, Four(t1, _, _, _) :: ds) => lookup(i - 4 * t1.size, ds)
  }

  override def update: (Int, E, SRList[E]) => SRList[E] = {
    case (_, _, Nil) => throw Subscript()
    case (i, y, Zero :: ds) => Zero :: update(i, y, ds)
    case (i, y, Ones(t :: ts) :: ds) if i < t.size => ones(updateTree(i, y, t) :: ts, ds)
    case (i, y, Ones(t :: ts) :: ds) => ones(List(t), update(i - t.size, y, ones(ts, ds)))
    case (i, y, Two(t1, t2) :: ds) if i < t1.size => Two(updateTree(i, y, t1), t2) :: ds
    case (i, y, Two(t1, t2) :: ds) if i < 2 * t1.size => Two(t1, updateTree(i - t1.size, y, t2)) :: ds
    case (i, y, Two(t1, t2) :: ds) => Two(t1, t2) :: update(i - 2 * t1.size, y, ds)
    case (i, y, Threes((t1, t2, t3) :: ts) :: ds) if i < t1.size => threes((updateTree(i, y, t1), t2, t3) :: ts, ds)
    case (i, y, Threes((t1, t2, t3) :: ts) :: ds) if i < 2 * t1.size => threes((t1, updateTree(i - t1.size, y, t2), t3) :: ts, ds)
    case (i, y, Threes((t1, t2, t3) :: ts) :: ds) if i < 3 * t1.size => threes((t1, t2, updateTree(i - 2 * t1.size, y, t3)) :: ts, ds)
    case (i, y, Threes((t1, t2, t3) :: ts) :: ds) => threes(List((t1, t2, t3)), update(i - 3 * t1.size, y, threes(ts, ds)))
    case (i, y, Four(t1, t2, t3, t4) :: ds) if i < t1.size => Four(updateTree(i, y, t1), t2, t3, t4) :: ds
    case (i, y, Four(t1, t2, t3, t4) :: ds) if i < 2 * t1.size => Four(t1, updateTree(i - t1.size, y, t2), t3, t4) :: ds
    case (i, y, Four(t1, t2, t3, t4) :: ds) if i < 3 * t1.size => Four(t1, t2, updateTree(i - 2 * t1.size, y, t3), t4) :: ds
    case (i, y, Four(t1, t2, t3, t4) :: ds) if i < 4 * t1.size => Four(t1, t2, t3, updateTree(i - 3 * t1.size, y, t4)) :: ds
    case (i, y, Four(t1, t2, t3, t4) :: ds) => Four(t1, t2, t3, t4) :: update(i - 4 * t1.size, y, ds)
  }
}
