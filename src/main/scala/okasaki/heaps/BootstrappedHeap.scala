package okasaki.heaps

import okasaki.Heap
import okasaki.heaps.BootstrappedHeap.{BSHeap, Empty, H}

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object BootstrappedHeap {

  sealed trait BSHeap[+A, +H[_]]

  object Empty extends BSHeap[Nothing, Nothing] {
    override def toString = "Empty"
  }

  case class H[A, BH[_]](x: A, bsh: BH[BSHeap[A, BH]]) extends BSHeap[A, BH] {
    override def toString = s"H($x, $bsh)"
  }

}

abstract class BootstrappedHeap[E, BaseHeap[X] <: Heap[X, BaseHeap[X]], This <: BootstrappedHeap[E, BaseHeap, This]](val h: BSHeap[E, BaseHeap] = Empty)
                                                                                                                    (implicit ord: Ordering[E])
  extends Heap[E, This] {

  implicit val hord: Ordering[BSHeap[E, BaseHeap]] =
    Ordering.by {
      case Empty => None
      case H(x, _) => Some(x)
    }

  def baseHeap: BaseHeap[BSHeap[E, BaseHeap]]

  def create(h: BSHeap[E, BaseHeap]): This

  override def empty = create(Empty)

  override def isEmpty: Boolean = h == Empty

  override def insert(x: E) = create(merge(H(x, baseHeap.empty), h))

  override def merge(o: This) = create(merge(h, o.h))

  private def merge(a: BSHeap[E, BaseHeap], b: BSHeap[E, BaseHeap]): BSHeap[E, BaseHeap] = (a, b) match {
    case (Empty, _) => b
    case (_, Empty) => a
    case (h1@H(x, p1), h2@H(y, p2)) =>
      if (ord.lteq(x, y)) H(x, p1.insert(h2))
      else H(y, p2.insert(h1))
  }

  override def findMin: E = h match {
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
    case H(x, _) => x
  }

  override def deleteMin = create(deleteMin(h))

  private def deleteMin(h: BSHeap[E, BaseHeap]): BSHeap[E, BaseHeap] = h match {
    case Empty =>
      throw new IllegalStateException("called deleteMin on an empty heap")
    case H(_, p) if p.isEmpty =>
      Empty
    case H(_, p) =>
      val H(y, p1) = p.findMin
      val p2 = p.deleteMin
      H(y, p1.merge(p2))
  }
}
