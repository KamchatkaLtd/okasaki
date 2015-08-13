package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object OutputRestrictedDeque {

  case class Repr[E, Q](q: Q, f: List[E])

  class ForQueue[E, Q](queue: Queue[E, Q]) extends OutputRestrictedDeque[E, Repr[E, Q]] {
    override def cons: (Repr[E, Q], E) => Repr[E, Q] = {
      case (Repr(q, f), x) => Repr(q, x :: f)
    }

    override def empty: Repr[E, Q] = Repr(queue.empty, Nil)

    override def snoc: (Repr[E, Q], E) => Repr[E, Q] = {
      case (Repr(q, f), x) => Repr(queue.snoc(q, x), f)
    }

    override def tail: (Repr[E, Q]) => Repr[E, Q] = {
      case Repr(q, Nil) => Repr(queue.tail(q), Nil)
      case Repr(q, _ :: f) => Repr(q, f)
    }

    override def isEmpty: (Repr[E, Q]) => Boolean = {
      case Repr(q, Nil) => queue.isEmpty(q)
      case _ => false
    }

    override def head: (Repr[E, Q]) => E = {
      case Repr(q, Nil) => queue.head(q)
      case Repr(_, x :: f) => x
    }
  }

}

trait OutputRestrictedDeque[E, Q] extends Queue[E, Q] {
  def cons: (Q, E) => Q
}
