package okasaki.heaps

import okasaki.Heap
import okasaki.misc.Susp

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object LazyPairingHeap {

  sealed trait Repr[+E]

  object Empty extends Repr[Nothing] {
    override def toString = s"E"
  }

  case class Tree[E](e: E, odd: Repr[E], lts: Susp[Repr[E]]) extends Repr[E] {
    override def toString: String = s"T($e,$odd,$lts)"
  }

}

class LazyPairingHeap[E](implicit val ord: Ordering[E]) extends Heap[E, LazyPairingHeap.Repr[E]] {

  import okasaki.heaps.LazyPairingHeap._

  override def empty: Repr[E] = Empty

  override def isEmpty(h: Repr[E]): Boolean = h == Empty

  def link(t1: Tree[E], t2: Tree[E]): Repr[E] = (t1, t2) match {
    case (Tree(x, Empty, m), a) => Tree(x, a, m)
    case (Tree(x, b, m), a) => Tree(x, Empty, Susp(merge(merge(a, b), m())))
  }

  override def merge(a: Repr[E], b: Repr[E]): Repr[E] = (a, b) match {
    case (Empty, h) => h
    case (h, Empty) => h
    case (aa@Tree(x, _, _), bb@Tree(y, _, _)) =>
      if (ord.lteq(x, y)) link(aa, bb) else link(bb, aa)
  }

  override def insert(x: E, t: Repr[E]): Repr[E] =
    merge(Tree(x, Empty, Susp[Repr[E]](Empty)), t)

  def mergePairs(hs: List[Repr[E]]): Repr[E] = hs match {
    case Nil => Empty
    case h :: Nil => h
    case h1 :: h2 :: rest => merge(merge(h1, h2), mergePairs(rest))
  }

  override def findMin(h: Repr[E]): E = h match {
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
    case Tree(x, _, _) => x
  }

  override def deleteMin(h: Repr[E]): Repr[E] = h match {
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
    case Tree(_, a, m) => merge(a, m())
  }
}
