package okasaki.heaps

import okasaki.Heap

import scala.collection.immutable.Stream.Empty

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object ScheduledBinomialHeap {

  sealed trait Digit[+E]

  object Zero extends Digit[Nothing]

  case class One[E](tree: Node[E]) extends Digit[E]

  case class Node[E](e: E, c: List[Node[E]])

  type Schedule[E] = List[Stream[Digit[E]]]

  type Repr[E] = (Stream[Digit[E]], Schedule[E])
}

class ScheduledBinomialHeap[E](implicit val ord: Ordering[E]) extends Heap[E, ScheduledBinomialHeap.Repr[E]] {

  import okasaki.heaps.ScheduledBinomialHeap._

  override def empty: Repr[E] = (Empty, Nil)

  override def isEmpty(h: Repr[E]): Boolean = h match {
    case (Empty, _) => true
    case _ => false
  }

  def link(t1: Node[E], t2: Node[E]): Node[E] = (t1, t2) match {
    case (Node(x1, c1), Node(x2, c2)) =>
      if (ord.lteq(x1, x2)) Node(x1, t2 :: c1) else Node(x2, t1 :: c2)
  }

  def insTree(t: Node[E], ts: Stream[Digit[E]]): Stream[Digit[E]] = ts match {
    case Empty => Stream(One(t))
    case Zero #:: ts1 => One(t) #:: ts1
    case One(t1) #:: ts1 => Zero #:: insTree(link(t, t1), ts1)
  }

  def mrg(a: Stream[Digit[E]], b: Stream[Digit[E]]): Stream[Digit[E]] = (a, b) match {
    case (ds1, Empty) => ds1
    case (Empty, ds2) => ds2
    case (Zero #:: ds1, d #:: ds2) => d #:: mrg(ds1, ds2)
    case (d #:: ds1, Zero #:: ds2) => d #:: mrg(ds1, ds2)
    case (One(t1) #:: ds1, One(t2) #:: ds2) => Zero #:: insTree(link(t1, t2), mrg(ds1, ds2))
  }

  // ex. 7.4
  def mrgWithList(a: List[Node[E]], b: Stream[Digit[E]]): Stream[Digit[E]] = (a, b) match {
    case (Nil, ds2) => ds2
    case (ds1, Empty) => ds1.map(One[E]).toStream
    case (d :: ds1, Zero #:: ds2) => One(d) #:: mrgWithList(ds1, ds2)
    case (t1 :: ds1, One(t2) #:: ds2) => Zero #:: insTree(link(t1, t2), mrgWithList(ds1, ds2))
  }

  def exec[T](s: Schedule[T]): Schedule[T] = s match {
    case Nil => Nil
    case (Zero #:: job) :: sched => job :: sched
    case _ :: sched => sched
  }

  override def insert(x: E, h: Repr[E]): Repr[E] = {
    val (ds, sched) = h
    val ds1 = insTree(Node(x, Nil), ds)
    (ds1, exec(exec(ds1 :: sched)))
  }

  override def merge(a: Repr[E], b: Repr[E]): Repr[E] = {
    val ((ds1, _), (ds2, _)) = (a, b)
    val ds = mrg(ds1, ds2)
    (ds.force, Nil)
  }

  def removeMinTree(ds: Stream[Digit[E]]): (Node[E], Stream[Digit[E]]) = ds match {
    case Empty => throw new IllegalStateException("called removeMinTree on an empty heap")
    case (One(t) #:: Empty) => (t, Empty)
    case (Zero #:: dss) =>
      val (t1, ds1) = removeMinTree(dss)
      (t1, Zero #:: ds1)
    case (One(t@Node(x, _)) #:: dss) =>
      removeMinTree(dss) match {
        case (t1@Node(x1, _), ds1) =>
          if (ord.lteq(x, x1)) (t, Zero #:: dss)
          else (t1, One(t) #:: ds1)
      }
  }

  override def findMin(h: Repr[E]): E = {
    val (ds, _) = h
    val (Node(x, _), _) = removeMinTree(ds)
    x
  }

  override def deleteMin(h: Repr[E]): Repr[E] = {
    val (ds, _) = h
    val (Node(_, c), ds1) = removeMinTree(ds)
    val ds11 = mrgWithList(c.reverse, ds1)
    (ds11.force, Nil)
  }
}
