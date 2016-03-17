package okasaki.queues

import okasaki.Queue
import okasaki.queues.BankersQueue.Repr

import scala.collection.immutable.Stream.Empty

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object BankersQueue {
  type Repr[E] = (Int, Stream[E], Int, Stream[E])
}

class BankersQueue[E] extends Queue[E, Repr[E]] {
  override def empty: Repr[E] = (0, Empty, 0, Empty)

  override def isEmpty(q: Repr[E]): Boolean = q._1 == 0

  def check(q: Repr[E]): Repr[E] = {
    val (lenf, f, lenr, r) = q
    if (lenr > lenf) (lenf + lenr, f #::: r.reverse, 0, Empty) else q
  }

  override def snoc(q: Repr[E], x: E): Repr[E] = q match {
    case (lenf, f, lenr, r) => check(lenf, f, lenr + 1, x #:: r)
  }

  override def head(q: Repr[E]): E = q match {
    case (0, _, _, _) => throw new IllegalStateException("head called on an empty queue")
    case (_, (x #:: _), _, _) => x
  }

  override def tail(q: Repr[E]): Repr[E] = q match {
    case (0, _, _, _) => throw new IllegalStateException("tail called on an empty queue")
    case (lenf, (_ #:: f), lenr, r) => check(lenf - 1, f, lenr, r)
  }
}
