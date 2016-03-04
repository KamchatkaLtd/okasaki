package okasaki.queues

import okasaki.Queue
import okasaki.misc.Susp
import okasaki.queues.PhysicistQueue._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object PhysicistQueue {

  case class Repr[E](w: List[E], lenf: Int, f: Susp[List[E]], lenr: Int, r: List[E])

}

class PhysicistQueue[E] extends Queue[E, Repr[E]] {
  override val empty: Repr[E] =
    Repr(Nil, 0, Susp(Nil), 0, Nil)

  override val isEmpty: (Repr[E]) => Boolean =
    _.lenf == 0

  def check(q: Repr[E]): Repr[E] =
    if (q.lenr <= q.lenf) checkw(q)
    else checkw(Repr(q.f(), q.lenf + q.lenr, Susp(q.f() ++ q.r.reverse), 0, Nil))

  def checkw(q: Repr[E]): Repr[E] =
    if (q.w.isEmpty) q.copy(w = q.f()) else q

  override val snoc: (Repr[E], E) => Repr[E] = (q, x) =>
    check(q.copy(r = x :: q.r, lenr = q.lenr + 1))

  override val tail: (Repr[E]) => Repr[E] = {
    case Repr(Nil, _, _, _, _) => throw new IllegalStateException("tail called on an empty queue")
    case q@Repr(w, lenf, f, _, _) => check(q.copy(w = w.tail, lenf = lenf - 1, f = Susp(f().tail)))
  }

  override val head: (Repr[E]) => E = {
    case Repr(Nil, _, _, _, _) => throw new IllegalStateException("head called on an empty queue")
    case q@Repr(x :: _, _, _, _, _) => x
  }
}
