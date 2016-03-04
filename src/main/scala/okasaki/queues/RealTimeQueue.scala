package okasaki.queues

import okasaki.Queue
import okasaki.queues.RealTimeQueue._

import scala.collection.immutable.Stream.Empty

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object RealTimeQueue {
  type Repr[E] = (Stream[E], List[E], Stream[E])
}

class RealTimeQueue[E] extends Queue[E, Repr[E]] {
  override def empty: Repr[E] = (Empty, Nil, Empty)

  override def isEmpty: (Repr[E]) => Boolean = _._1.isEmpty

  def rotate(q: Repr[E]): Stream[E] = q match {
    case (Empty, y :: _, a) => y #:: a
    case (x #:: xs, y :: ys, a) => x #:: rotate(xs, ys, y #:: a)
  }

  def exec(q: Repr[E]): Repr[E] = q match {
    case (f, r, x #:: s) => (f, r, s)
    case (f, r, Empty) =>
      val f1 = rotate(f, r, Empty)
      (f1, Nil, f1)
  }

  override def snoc: (Repr[E], E) => Repr[E] = {
    case ((f, r, s), x) => exec(f, x :: r, s)
  }

  override def head: (Repr[E]) => E = {
    case (Empty, _, _) => throw new IllegalStateException("head called on an empty queue")
    case (x #:: _, _, _) => x
  }

  override def tail: (Repr[E]) => Repr[E] = {
    case (Empty, _, _) => throw new IllegalStateException("tail called on an empty queue")
    case (_ #:: f, r, s) => exec(f, r, s)
  }
}
