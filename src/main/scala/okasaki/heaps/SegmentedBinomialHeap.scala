package okasaki.heaps

import okasaki.Heap

import scala.collection.immutable.::

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object SegmentedBinomialHeap {

  case class Node[E](e: E, c: List[Node[E]])


  sealed trait Digit[+E]

  case object Zero extends Digit[Nothing]

  case class Ones[E](ones: List[Node[E]]) extends Digit[E]

  case class Two[E](t1: Node[E], t2: Node[E]) extends Digit[E]


  type SBHeap[E] = List[Digit[E]]
}


class SegmentedBinomialHeap[E](implicit val ord: Ordering[E]) extends Heap[E, SegmentedBinomialHeap.SBHeap[E]] {

  import okasaki.heaps.SegmentedBinomialHeap._

  override def empty: SBHeap[E] = Nil

  override def isEmpty(h: SBHeap[E]): Boolean = h.isEmpty

  def root(t: Node[E]): E = t.e

  def link(t1: Node[E], t2: Node[E]): Node[E] = (t1, t2) match {
    case (Node(x1, c1), Node(x2, c2)) =>
      if (ord.lteq(x1, x2)) Node(x1, t2 :: c1)
      else Node(x2, t1 :: c2)
  }

  def ones(maybeNode: List[Node[E]], digits: List[Digit[E]]): List[Digit[E]] = (maybeNode, digits) match {
    case (Nil, ds) => ds
    case (ns1, Ones(ns2) :: ds) => Ones(ns1 ++ ns2) :: ds // assert ns1.size <= 1
    case (ns, ds) => Ones(ns) :: ds
  }

  def simpleIns(t: Node[E], ts: SBHeap[E]): SBHeap[E] = ts match {
    case Nil => ones(t :: Nil, Nil)
    case Zero :: ts1 => ones(t :: Nil, ts1)
    case Ones(d :: ds) :: ts1 => Two(t, d) :: ones(ds, ts1)
  }

  def fixup(h: SBHeap[E]): SBHeap[E] = h match {
    case Two(a, b) :: ds =>
      Zero :: simpleIns(link(a, b), ds)
    case Ones(ts) :: Two(a, b) :: ds =>
      Ones(ts) :: Zero :: simpleIns(link(a, b), ds)
    case ds => ds
  }

  def insTree(t: Node[E], ts: SBHeap[E]): SBHeap[E] = fixup(simpleIns(t, ts))

  override def insert(x: E, ts: SBHeap[E]): SBHeap[E] = insTree(Node(x, Nil), ts)

  override def merge(a: SBHeap[E], b: SBHeap[E]): SBHeap[E] = (a, b) match {
    case (ds1, Nil) => ds1
    case (Nil, ds2) => ds2
    case (Zero :: ds1, Zero :: ds2) =>
      Zero :: merge(ds1, ds2)
    case (Ones(t1 :: ts1) :: ds1, Zero :: ds2) =>
      ones(List(t1), merge(ones(ts1, ds1), ds2))
    case (Zero :: ds1, Ones(t2 :: ts2) :: ds2) =>
      ones(List(t2), merge(ds1, ones(ts2, ds2)))
    case (Two(a1, b1) :: ds1, Zero :: ds2) =>
      insTree(link(a1, b1), fixup(merge(ds1, ds2)))
    case (Zero :: ds1, Two(a2, b2) :: ds2) =>
      insTree(link(a2, b2), fixup(merge(ds1, ds2)))
    case (Ones(t1 :: ts1) :: ds1, Ones(t2 :: ts2) :: ds2) =>
      Zero :: insTree(link(t1, t2), fixup(merge(ones(ts1, ds1), ones(ts2, ds2))))
    case (Two(a1, b1) :: ds1, Ones(t2 :: ts2) :: ds2) =>
      ones(List(t2), fixup(insTree(link(a1, b1), fixup(merge(ds1, ones(ts2, ds2))))))
    case (Ones(t1 :: ts1) :: ds1, Two(a2, b2) :: ds2) =>
      ones(List(t1), insTree(link(a2, b2), fixup(merge(ones(ts1, ds1), ds2))))
    case (Two(a1, b1) :: ds1, Two(a2, b2) :: ds2) =>
      Zero :: insTree(link(a2, b2), insTree(link(a1, b1), fixup(merge(ds1, ds2))))
  }

  def min(d: Digit[E]): E = d match {
    case Ones(t :: Nil) => t.e
    case Ones(t :: ts) => ord.min(t.e, min(Ones(ts)))
    case Two(t1, t2) => ord.min(t1.e, t2.e)
  }

  // _.filterNot(_ == Zero).map(min).reduce(ord.min)
  override def findMin(h: SBHeap[E]): E = h match {
    case Nil => throw new IllegalStateException("called findMin on an empty heap")
    case Zero :: ds => findMin(ds)
    case d :: Nil => min(d)
    case d :: ds => ord.min(min(d), findMin(ds))
  }

  override def deleteMin(h: SBHeap[E]): SBHeap[E] = removeMinTree(h) match {
    case (Node(_, Nil), ts2) => ts2
    case (Node(_, ts1), ts2) => merge(fixup(ones(ts1.reverse, Nil)), ts2)
  }

  def decOnes(trees: List[Node[E]], rest: SBHeap[E]): (Node[E], SBHeap[E]) = trees match {
    case t :: Nil => (t, rest)
    case t :: ts =>
      decOnes(ts, rest) match {
        case (t1, ds1) =>
          if (ord.lteq(root(t), root(t1))) (t, Zero :: ones(ts, rest))
          else (t1, Zero :: ones(List(t), ds1))
      }
  }

  private def removeMinTree(h: SBHeap[E]): (Node[E], SBHeap[E]) = {
    def betterHeap(a: (Node[E], SBHeap[E]), b: (Node[E], SBHeap[E])): (Node[E], SBHeap[E]) = {
      val (t1, h1) = a
      val (t2, h2) = b
      if (ord.lteq(root(t1), root(t2))) (t1, h1)
      else (t2, h2)
    }

    def betterTree(t1: Node[E], t2: Node[E], rest: SBHeap[E]): (Node[E], SBHeap[E]) =
      if (ord.lteq(root(t1), root(t2))) (t1, ones(List(t2), rest))
      else (t2, ones(List(t1), rest))

    h match {
      case Nil => throw new IllegalStateException("called removeMinTree on an empty heap")
      case Zero :: ds =>
        val (t, ds1) = removeMinTree(ds)
        (t, if (isEmpty(ds1)) Nil else Zero :: ds1)
      case Ones(ts) :: Nil =>
        decOnes(ts, Nil)
      case Ones(ts) :: ds =>
        val (t, ds1) = removeMinTree(ds)
        betterHeap(decOnes(ts, ds), (t, ones(ts, ds1)))
      case Two(t1, t2) :: Nil =>
        betterTree(t1, t2, Nil)
      case Two(t1, t2) :: ds =>
        val (t, ds1) = removeMinTree(ds)
        betterHeap(betterTree(t1, t2, ds), (t, Two(t1, t2) :: ds1))
    }
  }
}
