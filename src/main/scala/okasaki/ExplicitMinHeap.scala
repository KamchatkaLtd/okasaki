package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ExplicitMinHeap[E, H](heap: Heap[E, H]) extends Heap[E, (Option[E], H)] {
  override implicit def ord: Ordering[E] = heap.ord

  override def insert: (E, (Option[E], H)) => (Option[E], H) = {
    case (e, (x, h)) => (min(x, Some(e)), heap.insert(e, h))
  }

  override def deleteMin: ((Option[E], H)) => (Option[E], H) = {
    case (None, _) => throw new IllegalStateException("called deleteMin on an empty heap")
    case (Some(x), h) =>
      val h1 = heap.deleteMin(h)
      val x1 = if (heap.isEmpty(h1)) None else Some(heap.findMin(h1))
      (x1, h1)
  }

  override def merge: ((Option[E], H), (Option[E], H)) => (Option[E], H) = {
    case ((x1, h1), (x2, h2)) => (min(x1, x2), heap.merge(h1, h2))
  }

  override def empty: (Option[E], H) = (None, heap.empty)

  override def isEmpty: ((Option[E], H)) => Boolean = {
    case (None, _) => true
    case _ => false
  }

  override def findMin: ((Option[E], H)) => E = {
    case (None, _) => throw new IllegalStateException("called findMin on an empty heap")
    case (Some(x), _) => x
  }

  def min(x: Option[E], y: Option[E]): Option[E] = (x, y) match {
    case (None, _) => y
    case (_, None) => x
    case (Some(xx), Some(yy)) => Some(ord.min(xx, yy))
  }
}
