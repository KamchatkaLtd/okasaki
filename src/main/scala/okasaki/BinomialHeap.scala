package okasaki

import okasaki.ConsList.{Empty, reverse}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object BinomialHeap {

  case class Node[E](r: Int, e: E, c: ConsList[Node[E]])(implicit list: List[E])

  type BHeap[E] = ConsList[Node[E]]
}

trait BinomialHeap[E] extends Heap[E, BinomialHeap.BHeap[E]] {

  import okasaki.BinomialHeap._

  def ord: Ordering[E]

  override def empty: BHeap[E] = Empty

  override def isEmpty: (BHeap[E]) => Boolean = {
    case Empty => true
    case _ => false
  }

  def rank(t: Node[E]): Int = t.r

  def root(t: Node[E]): E = t.e

  def link(t1: Node[E], t2: Node[E]): Node[E] = (t1, t2) match {
    case (Node(r, x1, c1), Node(_, x2, c2)) =>
      if (ord.lteq(x1, x2)) Node(r + 1, x1, Cons(t2, c1))
      else Node(r + 1, x2, Cons(t1, c2))
  }

  def insTree(t: Node[E], ts: BHeap[E]): BHeap[E] = ts match {
    case Empty => Cons(t, Empty)
    case Cons(t1, ts1) => if (rank(t) < rank(t1)) Cons(t, ts) else insTree(link(t, t1), ts1)
  }

  override def insert: (E, BHeap[E]) => BHeap[E] = {
    case (x, ts) => insTree(Node(0, x, Empty), ts)
  }

  override def merge: (BHeap[E], BHeap[E]) => BHeap[E] = {
    case (ts1, Empty) => ts1
    case (Empty, ts2) => ts2
    case (ts1@Cons(t1, ts11), ts2@Cons(t2, ts22)) =>
      if (rank(t1) < rank(t2)) Cons(t1, merge(ts11, ts2))
      else if (rank(t2) < rank(t1)) Cons(t2, merge(ts1, ts22))
      else insTree(link(t1, t2), merge(ts11, ts22))
  }

  override def findMin: (BHeap[E]) => E = {
    case Empty => throw new IllegalStateException("called removeMinTree on an empty heap")
    case Cons(t, Empty) => root(t)
    case Cons(t, ts) => ord.min(root(t), findMin(ts))
  }

  override def deleteMin: (BHeap[E]) => BHeap[E] = removeMinTree _ andThen {
    case (Node(_, x: E, ts1), ts2) => merge(reverse(ts1), ts2)
  }

  private def removeMinTree(h: BHeap[E]): (Node[E], BHeap[E]) = h match {
    case Empty => throw new IllegalStateException("called removeMinTree on an empty heap")
    case Cons(t, Empty) => (t, Empty)
    case Cons(t, ts) =>
      removeMinTree(ts) match {
        case (t1, ts1) =>
          if (ord.lteq(root(t), root(t1))) (t, ts)
          else (t1, Cons(t, ts1))
      }
  }
}
