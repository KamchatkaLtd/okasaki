package okasaki.heaps

import okasaki.Heap
import okasaki.heaps.SplayHeap.{Empty, SHeap}

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
    val heap = new SplayHeap[E](Empty)
    val h = list.foldLeft(heap)(_ insert _)
    h.toList
  }

}

class SplayHeap[E](val h: SHeap[E] = Empty)
                  (implicit val ord: Ordering[E])
  extends Heap[E, SplayHeap[E]] {

  import okasaki.heaps.SplayHeap._

  override def empty = new SplayHeap[E]

  override def isEmpty = h == Empty

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

  override def insert(x: E) = {
    val (a, b) = partition(x, h)
    new SplayHeap[E](Tree(a, x, b))
  }

  override def merge(o: SplayHeap[E]) = new SplayHeap[E](merge(h, o.h))

  private def merge(h1: SHeap[E], h2: SHeap[E]): SHeap[E] = (h1, h2) match {
    case (Empty, t) => t
    case (Tree(a, x, b), t) =>
      val (a1, b1) = partition(x, t)
      Tree(merge(a, a1), x, merge(b, b1))
  }

  override def findMin = findMin(h)

  private def findMin(h: SHeap[E]): E = h match {
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
    case Tree(Empty, x, _) => x
    case Tree(a, _, _) => findMin(a)
  }

  override def deleteMin = new SplayHeap[E](deleteMin(h))

  private def deleteMin(h: SHeap[E]): SHeap[E] = h match {
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
    case Tree(Empty, _, b) => b
    case Tree(Tree(Empty, _, b), y, c) => Tree(b, y, c)
    case Tree(Tree(a, x, b), y, c) => Tree(deleteMin(a), x, Tree(b, y, c))
  }

  private def inOrder(sHeap: SHeap[E], res: List[E]): List[E] = sHeap match {
    case Empty => res
    case Tree(a, x, b) => inOrder(a, x :: inOrder(b, res))
  }

  override def toList: List[E] = inOrder(h, Nil)
}
