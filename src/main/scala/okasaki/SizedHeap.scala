package okasaki

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
class SizedHeap[E, H <: Heap[E, H]](val s: Int, val h: H) extends Heap[E, SizedHeap[E, H]] {
  override implicit def ord: Ordering[E] = h.ord

  override def empty = new SizedHeap[E, H](0, h.empty)

  override def isEmpty = s == 0

  override def insert(e: E) = new SizedHeap[E, H](s + 1, h.insert(e))

  override def merge(o: SizedHeap[E, H]) = new SizedHeap[E, H](s + o.s, h.merge(o.h))

  override def findMin: E = h.findMin

  override def deleteMin = new SizedHeap[E, H](s - 1, h.deleteMin)
}
