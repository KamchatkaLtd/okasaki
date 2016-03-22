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

  private def merge[E](a: Repr[E], b: Repr[E])
                      (implicit ord: Ordering[E]): Repr[E] = (a, b) match {
    case (h, Empty) => h
    case (Empty, h) => h
    case (h1@SubHeap(w1, x, a1, b1), h2@SubHeap(w2, y, a2, b2)) =>
      val w = w1 + w2
      if (ord.lteq(x, y)) SubHeap(w, x, a1, merge(b1, h2))
      else SubHeap(w, y, a2, merge(h1, b2))
  }

  private def insert[E](x: E, h: Repr[E])
                       (implicit ord: Ordering[E]): Repr[E] = h match {
    case Empty => SubHeap(1, x, Empty, Empty)
    case SubHeap(w, y, a, b) =>
      if (ord.lteq(x, y)) SubHeap(w + 1, x, a, insert(y, b))
      else SubHeap(w + 1, y, a, insert(x, b))
  }


}

class WeightBiasedLeftistHeap[E](val h: Repr[E] = Empty)
                                (implicit val ord: Ordering[E])
  extends Heap[E, WeightBiasedLeftistHeap[E]] {

  override def empty = new WeightBiasedLeftistHeap[E]

  override def isEmpty = h == Empty

  override def merge(o: WeightBiasedLeftistHeap[E]) =
    new WeightBiasedLeftistHeap[E](WeightBiasedLeftistHeap.merge(h, o.h))

  override def insert(x: E) =
    new WeightBiasedLeftistHeap[E](WeightBiasedLeftistHeap.insert(x, h))

  override def findMin = h match {
    case SubHeap(_, x, _, _) => x
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  override def deleteMin = h match {
    case SubHeap(_, _, a, b) => new WeightBiasedLeftistHeap[E](WeightBiasedLeftistHeap.merge(a, b))
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
  }
}
