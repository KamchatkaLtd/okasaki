package okasaki

import scala.collection.immutable.Stream.Empty

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait BankersQueue[E] extends Queue[E, (Int, Stream[E], Int, Stream[E])] {
  override def empty: (Int, Stream[E], Int, Stream[E]) = (0, Empty, 0, Empty)

  override def isEmpty: ((Int, Stream[E], Int, Stream[E])) => Boolean = _._1 == 0

  val check: (Int, Stream[E], Int, Stream[E]) => (Int, Stream[E], Int, Stream[E]) = {
    case (lenf, f, lenr, r) if lenr > lenf => (lenf + lenr, f #::: r.reverse, 0, Empty)
    case q => q
  }

  override def snoc: ((Int, Stream[E], Int, Stream[E]), E) => (Int, Stream[E], Int, Stream[E]) = {
    case ((lenf, f, lenr, r), x) => check(lenf, f, lenr + 1, x #:: r)
  }

  override def head: ((Int, Stream[E], Int, Stream[E])) => E = {
    case (0, _, _, _) => throw new IllegalStateException("head called on an empty queue")
    case (_, (x #:: _), _, _) => x
  }

  override def tail: ((Int, Stream[E], Int, Stream[E])) => (Int, Stream[E], Int, Stream[E]) = {
    case (0, _, _, _) => throw new IllegalStateException("tail called on an empty queue")
    case (lenf, (_ #:: f), lenr, r) => check(lenf - 1, f, lenr, r)
  }
}
