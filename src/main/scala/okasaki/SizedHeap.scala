package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SizedHeap[E, H](heap: Heap[E, H]) extends Heap[E, (Int, H)] {
  override implicit def ord: Ordering[E] = heap.ord

  override def insert: (E, (Int, H)) => (Int, H) = {
    case (e, (s, h)) => (s + 1, heap.insert(e, h))
  }

  override def deleteMin: ((Int, H)) => (Int, H) = {
    case (0, _) => throw new IllegalStateException("called deleteMin on an empty heap")
    case (s, h) => (s - 1, heap.deleteMin(h))
  }

  override def merge: ((Int, H), (Int, H)) => (Int, H) = {
    case ((s1, h1), (s2, h2)) => (s1 + s2, heap.merge(h1, h2))
  }

  override def empty: (Int, H) = (0, heap.empty)

  override def isEmpty: ((Int, H)) => Boolean = {
    case (0, _) => true
    case _ => false
  }

  override def findMin: ((Int, H)) => E = {
    case (0, _) => throw new IllegalStateException("called findMin on an empty heap")
    case (_, h) => heap.findMin(h)
  }
}
