package okasaki

import okasaki.ExplicitMinHeap._

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object ExplicitMinHeap {

  def findMinOpt[A](h: Heap[A, _]): Option[A] = {
    if (h.isEmpty) None else Some(h.findMin)
  }

  def min[A](x: Option[A], y: Option[A])(implicit ord: Ordering[A]): Option[A] = (x, y) match {
    case (None, _) => y
    case (_, None) => x
    case (Some(xx), Some(yy)) => Some(ord.min(xx, yy))
  }

}

class ExplicitMinHeap[E, H <: Heap[E, H]](val m: Option[E], val h: H) extends Heap[E, ExplicitMinHeap[E, H]] {
  override implicit def ord: Ordering[E] = h.ord

  def this(h: H) = this(if (h.isEmpty) None else Some(h.findMin), h)

  override def insert(e: E) = new ExplicitMinHeap[E, H](min(m, Some(e)), h.insert(e))

  override def deleteMin = if (m.isDefined) {
    val h1 = h.deleteMin
    new ExplicitMinHeap[E, H](findMinOpt(h1), h1)
  } else {
    throw new IllegalStateException("called deleteMin on an empty heap")
  }

  override def merge(o: ExplicitMinHeap[E, H]) =
    new ExplicitMinHeap[E, H](min(m, o.m), h.merge(o.h))

  override def empty = new ExplicitMinHeap[E, H](None, h.empty)

  override def isEmpty: Boolean = m.isEmpty

  override def findMin: E = m.getOrElse(throw new IllegalStateException("called findMin on an empty heap"))
}
