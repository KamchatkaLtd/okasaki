package okasaki

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
class ExplicitMinHeap[E, H](heap: Heap[E, H]) extends Heap[E, (Option[E], H)] {
  override implicit def ord: Ordering[E] = heap.ord

  override def insert(e: E, emh: (Option[E], H)): (Option[E], H) = emh match {
    case (x, h) => (min(x, Some(e)), heap.insert(e, h))
  }

  override def deleteMin(emh: (Option[E], H)): (Option[E], H) = emh match {
    case (None, _) => throw new IllegalStateException("called deleteMin on an empty heap")
    case (Some(x), h) =>
      val h1 = heap.deleteMin(h)
      val x1 = if (heap.isEmpty(h1)) None else Some(heap.findMin(h1))
      (x1, h1)
  }

  override def merge(a: (Option[E], H), b: (Option[E], H)): (Option[E], H) = (a, b) match {
    case ((x1, h1), (x2, h2)) => (min(x1, x2), heap.merge(h1, h2))
  }

  override def empty: (Option[E], H) = (None, heap.empty)

  override def isEmpty(emh: (Option[E], H)): Boolean = emh._1.isEmpty

  override def findMin(emh: (Option[E], H)): E = emh match {
    case (None, _) => throw new IllegalStateException("called findMin on an empty heap")
    case (Some(x), _) => x
  }

  def min(x: Option[E], y: Option[E]): Option[E] = (x, y) match {
    case (None, _) => y
    case (_, None) => x
    case (Some(xx), Some(yy)) => Some(ord.min(xx, yy))
  }
}
