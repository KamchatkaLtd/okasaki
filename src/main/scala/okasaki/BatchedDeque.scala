package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait BatchedDeque[E] extends Deque[E, (List[E], List[E])] {
  override def empty: (List[E], List[E]) = (Nil, Nil)

  /*
   * The invariant is as follows:
   *
   * We try to keep both ends non-empty, i.e. for size
   * 0 we have (Nil, Nil)
   * 1 we have (x :: Nil, Nil)
   * 2 and up - we have two non-empty lists
   */
  override def isEmpty: ((List[E], List[E])) => Boolean = {
    case (Nil, _) => true
    case _ => false
  }

  val checkf: (List[E], List[E]) => (List[E], List[E]) = {
    case (Nil, r) =>
      val (newr, newf) = r.splitAt(r.size / 2)
      (newf.reverse, newr)
    case q => q
  }

  val checkr: (List[E], List[E]) => (List[E], List[E]) = {
    case (f, Nil) =>
      val (newf, newr) = f.splitAt((f.size + 1) / 2)
      (newf, newr.reverse)
    case q => q
  }

  override def snoc: ((List[E], List[E]), E) => (List[E], List[E]) = {
    case ((f, r), x) => checkf(f, x :: r)
  }

  override def tail: ((List[E], List[E])) => (List[E], List[E]) = {
    case (Nil, _) => throw new IllegalStateException("tail called on an empty deque")
    case ((_ :: f), r) => checkf(f, r)
  }

  override def head: ((List[E], List[E])) => E = {
    case (Nil, _) => throw new IllegalStateException("head called on an empty deque")
    case ((x :: _), _) => x
  }

  override def cons: ((List[E], List[E]), E) => (List[E], List[E]) = {
    case ((f, r), x) => checkr(x :: f, r)
  }

  override def init: ((List[E], List[E])) => (List[E], List[E]) = {
    case (Nil, Nil) => throw new IllegalStateException("tail called on an empty deque")
    case (f, (_ :: r)) => checkr(f, r)
    case (x :: Nil, Nil) => (Nil, Nil)
  }

  override def last: ((List[E], List[E])) => E = {
    case (Nil, Nil) => throw new IllegalStateException("head called on an empty deque")
    case (_, (x :: _)) => x
    case (x :: Nil, Nil) => x
  }
}
