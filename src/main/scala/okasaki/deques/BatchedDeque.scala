package okasaki.deques

import okasaki.Deque
import okasaki.deques.BatchedDeque._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object BatchedDeque {
  type Repr[E] = (List[E], List[E])
}

class BatchedDeque[E] extends Deque[E, Repr[E]] {
  override def empty: Repr[E] = (Nil, Nil)

  /*
   * The invariant is as follows:
   *
   * We try to keep both ends non-empty, i.e. for size
   * 0 we have (Nil, Nil)
   * 1 we have (x :: Nil, Nil)
   * 2 and up - we have two non-empty lists
   */
  override def isEmpty(q: Repr[E]): Boolean = q match {
    case (Nil, _) => true
    case _ => false
  }

  def checkf(q: Repr[E]): Repr[E] = q match {
    case (Nil, r) =>
      val (newr, newf) = r.splitAt(r.size / 2)
      (newf.reverse, newr)
    case _ => q
  }

  def checkr(q: Repr[E]): Repr[E] = q match {
    case (f, Nil) =>
      val (newf, newr) = f.splitAt((f.size + 1) / 2)
      (newf, newr.reverse)
    case _ => q
  }

  override def snoc(q: Repr[E], x: E): Repr[E] = {
    val (f, r) = q
    checkf(f, x :: r)
  }

  override def tail(q: Repr[E]): Repr[E] = q match {
    case (Nil, _) => throw new IllegalStateException("tail called on an empty deque")
    case ((_ :: f), r) => checkf(f, r)
  }

  override def head(q: Repr[E]): E = q match {
    case (Nil, _) => throw new IllegalStateException("head called on an empty deque")
    case ((x :: _), _) => x
  }

  override def cons(x: E, q: (List[E], List[E])): Repr[E] = q match {
    case (f, r) => checkr(x :: f, r)
  }

  override def init(q: Repr[E]): Repr[E] = q match {
    case (Nil, _) => throw new IllegalStateException("init called on an empty deque")
    case (f, (_ :: r)) => checkr(f, r)
    case (x :: Nil, Nil) => (Nil, Nil)
    case (f, r) => throw new IllegalStateException(s"non-balanced deque: (f=$f,r=$r)")
  }

  override def last(q: Repr[E]): E = q match {
    case (Nil, _) => throw new IllegalStateException("last called on an empty deque")
    case (_, (x :: _)) => x
    case (x :: Nil, Nil) => x
    case (f, r) => throw new IllegalStateException(s"non-balanced deque: (f=$f,r=$r)")
  }
}
