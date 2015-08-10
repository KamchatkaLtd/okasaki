package okasaki

import okasaki.HoodMelvilleQueue._

object HoodMelvilleQueue {

  sealed trait RotationState[+E]

  object Idle extends RotationState[Nothing]

  case class Reversing[E](ok: Int, f: List[E], newf: List[E], r: List[E], newr: List[E]) extends RotationState[E]

  case class Appending[E](ok: Int, f: List[E], r: List[E]) extends RotationState[E]

  case class Done[E](newf: List[E]) extends RotationState[E]


  case class Repr[E](lenf: Int, f: List[E], state: RotationState[E], lenr: Int, r: List[E]) {
    def size = lenf + lenr
  }

}

class HoodMelvilleQueue[E] extends Queue[E, Repr[E]] {
  override def empty: Repr[E] = Repr(0, Nil, Idle, 0, Nil)

  override def isEmpty: (Repr[E]) => Boolean = _.lenf == 0

  def exec(state: RotationState[E]): RotationState[E] = state match {
    case Reversing(ok, x :: f, f1, y :: r, r1) => Reversing(ok + 1, f, x :: f1, r, y :: r1)
    case Reversing(ok, Nil, f1, y :: Nil, r1) => Appending(ok, f1, y :: r1)
    case Appending(0, _, r1) => Done(r1)
    case Appending(ok, x :: f1, r1) => Appending(ok - 1, f1, x :: r1)
    case _ => state
  }

  def invalidate(state: RotationState[E]): RotationState[E] = state match {
    case Reversing(ok, f, f1, r, r1) => Reversing(ok - 1, f, f1, r, r1)
    case Appending(0, _, _ :: r1) => Done(r1)
    case Appending(ok, f1, r1) => Appending(ok - 1, f1, r1)
    case _ => state
  }

  def exec2(q: Repr[E]): Repr[E] = {
    val Repr(lenf, f, state, lenr, r) = q
    exec(exec(state)) match {
      case Done(newf) => Repr(lenf, newf, Idle, lenr, r)
      case newstate => Repr(lenf, f, newstate, lenr, r)
    }
  }

  def check(q: Repr[E]): Repr[E] =
    if (q.lenr <= q.lenf) exec2(q)
    else {
      val newstate = Reversing(0, q.f, Nil, q.r, Nil)
      exec2(Repr(q.size, q.f, newstate, 0, Nil))
    }

  override def snoc: (Repr[E], E) => Repr[E] = {
    case (Repr(lenf, f, state, lenr, r), x) => check(Repr(lenf, f, state, lenr + 1, x :: r))
  }

  override def head: (Repr[E]) => E = {
    case Repr(_, Nil, _, _, _) => throw new IllegalStateException("head called on an empty queue")
    case Repr(_, x :: _, _, _, _) => x
  }

  override def tail: (Repr[E]) => Repr[E] = {
    case Repr(_, Nil, _, _, _) => throw new IllegalStateException("tail called on an empty queue")
    case Repr(lenf, _ :: f, state, lenr, r) =>
      check(Repr(lenf - 1, f, invalidate(state), lenr, r))
  }

}
