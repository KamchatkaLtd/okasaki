package okasaki

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
class HeapWithDeletion[E, H](heap: Heap[E, H]) extends Heap[E, (H, H)] {
  override implicit def ord: Ordering[E] = heap.ord

  private def normalize(pos: H, neg: H): (H, H) =
    if (heap.isEmpty(pos)) empty
    else if (heap.isEmpty(neg)) (pos, heap.empty)
    else {
      val minPos = heap.findMin(pos)
      val minNeg = heap.findMin(neg)
      if (ord.lt(minPos, minNeg)) (pos, neg)
      else if (ord.equiv(minPos, minNeg)) normalize(heap.deleteMin(pos), heap.deleteMin(neg))
      else normalize(pos, heap.deleteMin(neg))
    }

  def delete(e: E, h: (H, H)) = h match {
    case (pos, neg) =>
      val min = heap.findMin(pos)
      if (ord.lt(e, min))
        h
      else if (ord.equiv(e, min))
        normalize(heap.deleteMin(pos), neg)
      else
        (pos, heap.insert(e, neg))
  }

  override def insert(e: E, h: (H, H)): (H, H) = h match {
    case (pos, neg) => (heap.insert(e, pos), neg)
  }

  override def deleteMin(h: (H, H)): (H, H) = h match {
    case (pos, neg) => normalize(heap.deleteMin(pos), neg)
  }

  override def merge(a: (H, H), b: (H, H)): (H, H) = (a, b) match {
    case ((p1, n1), (p2, n2)) => (heap.merge(p1, p2), heap.merge(n1, n2))
  }

  override def empty: (H, H) = (heap.empty, heap.empty)

  override def isEmpty(h: (H, H)): Boolean = h match {
    case (pos, _) => heap.isEmpty(pos)
  }

  override def findMin(h: (H, H)): E = h match {
    case (pos, _) => heap.findMin(pos)
  }
}
