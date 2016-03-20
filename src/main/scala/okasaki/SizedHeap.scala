package okasaki

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
class SizedHeap[E, H](heap: Heap[E, H]) extends Heap[E, (Int, H)] {
  override implicit def ord: Ordering[E] = heap.ord

  override def insert(e: E, sh: (Int, H)): (Int, H) = sh match {
    case (s, h) => (s + 1, heap.insert(e, h))
  }

  override def deleteMin(sh: (Int, H)): (Int, H) = sh match {
    case (0, _) => throw new IllegalStateException("called deleteMin on an empty heap")
    case (s, h) => (s - 1, heap.deleteMin(h))
  }

  override def merge(a: (Int, H), b: (Int, H)): (Int, H) = (a, b) match {
    case ((s1, h1), (s2, h2)) => (s1 + s2, heap.merge(h1, h2))
  }

  override def empty: (Int, H) = (0, heap.empty)

  override def isEmpty(sh: (Int, H)): Boolean = sh match {
    case (0, _) => true
    case _ => false
  }

  override def findMin(sh: (Int, H)): E = sh match {
    case (0, _) => throw new IllegalStateException("called findMin on an empty heap")
    case (_, h) => heap.findMin(h)
  }
}
