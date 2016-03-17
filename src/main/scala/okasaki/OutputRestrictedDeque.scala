package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object OutputRestrictedDeque {

  case class Repr[E, Q](q: Q, f: List[E])

  class ForQueue[E, Q](queue: Queue[E, Q]) extends OutputRestrictedDeque[E, Repr[E, Q]] {
    override def cons(q: Repr[E, Q], x: E): Repr[E, Q] = q match {
      case Repr(q1, f) => Repr(q1, x :: f)
    }

    override def empty: Repr[E, Q] = Repr(queue.empty, Nil)

    override def snoc(q: Repr[E, Q], x: E): Repr[E, Q] = q match {
      case Repr(q1, f) => Repr(queue.snoc(q1, x), f)
    }

    override def tail(q: Repr[E, Q]): Repr[E, Q] = q match {
      case Repr(q1, Nil) => Repr(queue.tail(q1), Nil)
      case Repr(q1, _ :: f) => Repr(q1, f)
    }

    override def isEmpty(q: Repr[E, Q]): Boolean = q match {
      case Repr(q1, Nil) => queue.isEmpty(q1)
      case _ => false
    }

    override def head(q: Repr[E, Q]): E = q match {
      case Repr(q1, Nil) => queue.head(q1)
      case Repr(_, x :: f) => x
    }
  }

}

trait OutputRestrictedDeque[E, Q] extends Queue[E, Q] {
  def cons(q: Q, e: E): Q
}
