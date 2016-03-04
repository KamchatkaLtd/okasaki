package okasaki.heaps

import okasaki.Heap
import okasaki.heaps.BinomialHeap._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object BinomialHeap {

  case class Node[E](e: E, c: List[Node[E]])

  type BHeap[E] = List[(Int, Node[E])]
}

class BinomialHeap[E](implicit val ord: Ordering[E]) extends Heap[E, BinomialHeap.BHeap[E]] {

  override def empty: BHeap[E] = Nil

  override def isEmpty: (BHeap[E]) => Boolean = {
    case Nil => true
    case _ => false
  }

  def rank(t: (Int, Node[E])): Int = t._1

  def root(t: (Int, Node[E])): E = t._2.e

  def link(t1: (Int, Node[E]), t2: (Int, Node[E])): (Int, Node[E]) = (t1, t2) match {
    case ((r, Node(x1, c1)), (_, Node(x2, c2))) =>
      if (ord.lteq(x1, x2)) (r + 1, Node(x1, t2._2 :: c1))
      else (r + 1, Node(x2, t1._2 :: c2))
  }

  def insTree(t: (Int, Node[E]), ts: BHeap[E]): BHeap[E] = ts match {
    case Nil => List(t)
    case t1 :: ts1 => if (rank(t) < rank(t1)) t :: ts else insTree(link(t, t1), ts1)
  }

  override def insert: (E, BHeap[E]) => BHeap[E] = {
    case (x, ts) => insTree((0, Node(x, Nil)), ts)
  }

  override def merge: (BHeap[E], BHeap[E]) => BHeap[E] = {
    case (ts1, Nil) => ts1
    case (Nil, ts2) => ts2
    case (ts1@(t1 :: ts11), ts2@(t2 :: ts22)) =>
      if (rank(t1) < rank(t2)) t1 :: merge(ts11, ts2)
      else if (rank(t2) < rank(t1)) t2 :: merge(ts1, ts22)
      else insTree(link(t1, t2), merge(ts11, ts22))
  }

  override def findMin: (BHeap[E]) => E = {
    case Nil => throw new IllegalStateException("called findMin on an empty heap")
    case t :: Nil => root(t)
    case t :: ts => ord.min(root(t), findMin(ts))
  }

  override def deleteMin: (BHeap[E]) => BHeap[E] = removeMinTree _ andThen {
    case ((r, Node(x, ts1)), ts2) => merge(ts1.map(withRank(r - 1)).reverse, ts2)
  }

  def withRank(r: Int)(x: Node[E]): (Int, Node[E]) = (r, x)

  private def removeMinTree(h: BHeap[E]): ((Int, Node[E]), BHeap[E]) = h match {
    case Nil => throw new IllegalStateException("called removeMinTree on an empty heap")
    case t :: Nil => (t, Nil)
    case t :: ts =>
      removeMinTree(ts) match {
        case (t1, ts1) =>
          if (ord.lteq(root(t), root(t1))) (t, ts)
          else (t1, t :: ts1)
      }
  }
}
