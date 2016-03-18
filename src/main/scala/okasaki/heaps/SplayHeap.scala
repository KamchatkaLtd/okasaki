package okasaki.heaps

import okasaki.Heap

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object SplayHeap {

  sealed trait SHeap[+E]

  object Empty extends SHeap[Nothing] {
    override def toString = s"E"
  }

  case class Tree[E](l: SHeap[E], e: E, r: SHeap[E]) extends SHeap[E] {
    override def toString: String = s"T($l,$e,$r)"
  }

  def sort[E](list: List[E])(implicit ord: Ordering[E]): List[E] = {
    def inOrder(sHeap: SHeap[E], res: List[E]): List[E] = sHeap match {
      case Empty => res
      case Tree(a, x, b) => inOrder(a, x :: inOrder(b, res))
    }
    val heap = new SplayHeap[E]()
    val h = list.foldRight(heap.empty)(heap.insert)
    inOrder(h, Nil)
  }

}

class SplayHeap[E](implicit val ord: Ordering[E]) extends Heap[E, SplayHeap.SHeap[E]] {

  import okasaki.heaps.SplayHeap._

  override def empty: SHeap[E] = Empty

  override def isEmpty(h: SHeap[E]): Boolean = h == Empty

  def partition(pivot: E, h: SHeap[E]): (SHeap[E], SHeap[E]) = h match {
    case Empty => (Empty, Empty)
    case t@Tree(a, x, b) =>
      if (ord.lteq(x, pivot)) b match {
        case Empty => (t, Empty)
        case Tree(b1, y, b2) =>
          if (ord.lteq(y, pivot)) {
            val (small, big) = partition(pivot, b2)
            (Tree(Tree(a, x, b1), y, small), big)
          } else {
            val (small, big) = partition(pivot, b1)
            (Tree(a, x, small), Tree(big, y, b2))
          }
      } else a match {
        case Empty => (Empty, t)
        case Tree(a1, y, a2) =>
          if (ord.lteq(y, pivot)) {
            val (small, big) = partition(pivot, a2)
            (Tree(a1, y, small), Tree(big, x, b))
          } else {
            val (small, big) = partition(pivot, a1)
            (small, Tree(big, y, Tree(a2, x, b)))
          }
      }

  }

  override def insert(x: E, t: SHeap[E]): SHeap[E] = {
    val (a, b) = partition(x, t)
    Tree(a, x, b)
  }

  override def merge(h1: SHeap[E], h2: SHeap[E]): SHeap[E] = (h1, h2) match {
    case (Empty, t) => t
    case (Tree(a, x, b), t) =>
      val (a1, b1) = partition(x, t)
      Tree(merge(a, a1), x, merge(b, b1))
  }

  override def findMin(h: SHeap[E]): E = h match {
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
    case Tree(Empty, x, _) => x
    case Tree(a, _, _) => findMin(a)
  }

  override def deleteMin(h: SHeap[E]): SHeap[E] = h match {
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
    case Tree(Empty, _, b) => b
    case Tree(Tree(Empty, _, b), y, c) => Tree(b, y, c)
    case Tree(Tree(a, x, b), y, c) => Tree(deleteMin(a), x, Tree(b, y, c))
  }
}
