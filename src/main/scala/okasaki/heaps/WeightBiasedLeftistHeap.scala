package okasaki.heaps

import okasaki.Heap
import okasaki.heaps.WeightBiasedLeftistHeap._


/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object WeightBiasedLeftistHeap {

  object Empty extends Repr[Nothing] {
    override def toString: String = "E"
  }

  case class SubHeap[E](weight: Long, x: E, left: Repr[E], right: Repr[E]) extends Repr[E] {
    override def toString: String = s"T($weight,$x,$left,$right)"
  }

  sealed trait Repr[+E]

}

class WeightBiasedLeftistHeap[E](implicit val ord: Ordering[E]) extends Heap[E, Repr[E]] {

  override val empty: Repr[E] = Empty

  override def isEmpty(h: Repr[E]): Boolean = h == Empty

  override def merge(a: Repr[E], b: Repr[E]): Repr[E] = (a, b) match {
    case (h, Empty) => h
    case (Empty, h) => h
    case (h1@SubHeap(w1, x, a1, b1), h2@SubHeap(w2, y, a2, b2)) =>
      val w = w1 + w2
      if (ord.lteq(x, y)) SubHeap(w, x, a1, merge(b1, h2))
      else SubHeap(w, y, a2, merge(h1, b2))
  }

  override def insert(x: E, h: Repr[E]): Repr[E] = h match {
    case Empty => SubHeap(1, x, Empty, Empty)
    case SubHeap(w, y, a, b) =>
      if (ord.lteq(x, y)) SubHeap(w + 1, x, a, insert(y, b))
      else SubHeap(w + 1, y, a, insert(x, b))
  }

  override def findMin(h: Repr[E]): E = h match {
    case SubHeap(_, x, _, _) => x
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  override def deleteMin(h: Repr[E]): Repr[E] = h match {
    case SubHeap(_, _, a, b) => merge(a, b)
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
  }
}
