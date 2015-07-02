package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait BatchedQueue[E] extends Queue[E, (List[E], List[E])] {
  override def empty: (List[E], List[E]) = (Nil, Nil)

  override def isEmpty: ((List[E], List[E])) => Boolean = {
    case (Nil, _) => true
    case _ => false
  }

  val checkf: (List[E], List[E]) => (List[E], List[E]) = {
    case (Nil, r) => (r.reverse, Nil)
    case q => q
  }

  override def snoc: ((List[E], List[E]), E) => (List[E], List[E]) = {
    case ((f, r), x) => checkf(f, x :: r)
  }

  override def tail: ((List[E], List[E])) => (List[E], List[E]) = {
    case (Nil, _) => throw new IllegalStateException("tail called on an empty queue")
    case ((_ :: f), r) => checkf(f, r)
  }

  override def head: ((List[E], List[E])) => E = {
    case (Nil, _) => throw new IllegalStateException("head called on an empty queue")
    case ((x :: _), _) => x
  }
}
