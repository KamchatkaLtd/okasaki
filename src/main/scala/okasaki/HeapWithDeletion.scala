package okasaki

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
class HeapWithDeletion[E, H <: Heap[E, H]](val pos: H, val neg: H) extends Heap[E, HeapWithDeletion[E, H]] {
  override implicit def ord: Ordering[E] = pos.ord

  def this(h: H) = this(h, h.empty)

  private def create(pos: H, neg: H): HeapWithDeletion[E, H] = new HeapWithDeletion[E, H](pos, neg)

  private def normalize(pos: H, neg: H): HeapWithDeletion[E, H] =
    if (pos.isEmpty) empty
    else if (neg.isEmpty) create(pos, pos.empty)
    else {
      val minPos = pos.findMin
      val minNeg = neg.findMin
      if (ord.lt(minPos, minNeg)) create(pos, neg)
      else if (ord.equiv(minPos, minNeg)) normalize(pos.deleteMin, neg.deleteMin)
      else normalize(pos, neg.deleteMin)
    }

  def delete(e: E): HeapWithDeletion[E, H] = {
    val min = pos.findMin
    if (ord.lt(e, min))
      this
    else if (ord.equiv(e, min))
      normalize(pos.deleteMin, neg)
    else
      new HeapWithDeletion[E, H](pos, neg.insert(e))
  }

  override def insert(e: E) = create(pos.insert(e), neg)

  override def deleteMin = normalize(pos.deleteMin, neg)

  override def merge(o: HeapWithDeletion[E, H]) = create(pos.merge(o.pos), neg.merge(o.neg))

  override def empty = create(pos.empty, pos.empty)

  override def isEmpty: Boolean = pos.isEmpty

  override def findMin: E = pos.findMin
}
