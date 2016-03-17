package okasaki.queues

import okasaki.Queue
import okasaki.queues.BatchedDeque._

object BatchedDeque {
  type Repr[E] = (List[E], List[E])
}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BatchedQueue[E] extends Queue[E, Repr[E]] {
  override def empty: Repr[E] = (Nil, Nil)

  override def isEmpty(q: Repr[E]): Boolean = q match {
    case (Nil, _) => true
    case _ => false
  }

  def checkf(f: List[E], r: List[E]): Repr[E] =
    if (f.isEmpty) (r.reverse, Nil)
    else (f, r)

  override def snoc(q: Repr[E], x: E): Repr[E] = q match {
    case (f, r) => checkf(f, x :: r)
  }

  override def tail(q: Repr[E]): Repr[E] = q match {
    case (Nil, _) => throw new IllegalStateException("tail called on an empty queue")
    case ((_ :: f), r) => checkf(f, r)
  }

  override def head(q: Repr[E]): E = q match {
    case (Nil, _) => throw new IllegalStateException("head called on an empty queue")
    case ((x :: _), _) => x
  }
}
