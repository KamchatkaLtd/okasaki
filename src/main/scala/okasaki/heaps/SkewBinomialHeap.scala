package okasaki.heaps

import okasaki.Heap
import okasaki.heaps.SkewBinomialHeap._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object SkewBinomialHeap {

  case class Node[E](r: Int, x: E, xs: List[E], c: List[Node[E]])

  type H[E] = List[Node[E]]
}

class SkewBinomialHeap[E](implicit val ord: Ordering[E]) extends Heap[E, H[E]] {
  override def empty: H[E] = Nil

  override def isEmpty(h: H[E]): Boolean = h.isEmpty

  private def link(t1: Node[E], t2: Node[E]): Node[E] = (t1, t2) match {
    case (Node(r, x1, xs1, c1), Node(_, x2, xs2, c2)) =>
      if (ord.lteq(x1, x2)) Node(r + 1, x1, xs1, t2 :: c1)
      else Node(r + 1, x2, xs2, t1 :: c2)
  }

  private def skewlink(x: E, t1: Node[E], t2: Node[E]): Node[E] = {
    val Node(r, y, ys, c) = link(t1, t2)
    if (ord.lteq(x, y)) Node(r, x, y :: ys, c)
    else Node(r, y, x :: ys, c)
  }

  private def insTree(t: Node[E], h: H[E]): H[E] = h match {
    case Nil => List(t)
    case t2 :: ts => if (t.r < t2.r) t :: t2 :: ts else insTree(link(t, t2), ts)
  }

  private def mergeTrees(ts1: H[E], ts2: H[E]): H[E] = (ts1, ts2) match {
    case (_, Nil) => ts1
    case (Nil, _) => ts2
    case (t1 :: ts11, t2 :: _) if t1.r < t2.r => t1 :: mergeTrees(ts11, ts2)
    case (t1 :: _, t2 :: ts22) if t1.r > t2.r => t2 :: mergeTrees(ts1, ts22)
    case (t1 :: ts11, t2 :: ts22) => link(t1, t2) :: mergeTrees(ts11, ts22)
  }

  private def normalize(h: H[E]): H[E] = h match {
    case Nil => Nil
    case t :: ts => insTree(t, ts)
  }

  override def insert(x: E, h: H[E]): H[E] = h match {
    case t1 :: t2 :: rest if t1.r == t2.r => skewlink(x, t1, t2) :: rest
    case ts => Node(0, x, Nil, Nil) :: ts
  }

  override def merge(ts1: H[E], ts2: H[E]): H[E] =
    mergeTrees(normalize(ts1), normalize(ts2))

  private def removeMinTree(h: H[E]): (Node[E], H[E]) = h match {
    case Nil => throw new IllegalStateException("Empty heap")
    case t :: Nil => (t, Nil)
    case t :: ts =>
      val (t1, ts1) = removeMinTree(ts)
      if (ord.lteq(t.x, t1.x)) (t, ts) else (t1, t :: ts1)
  }

  override def findMin(h: H[E]): E = removeMinTree(h)._1.x

  override def deleteMin(h: H[E]): H[E] = {
    val (Node(_, x, xs, ts1), ts2) = removeMinTree(h)
    val ts = merge(ts1.reverse, ts2)
    xs.foldLeft(ts)((h, value) => insert(value, h))
  }
}
