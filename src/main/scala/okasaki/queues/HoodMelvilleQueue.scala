package okasaki.queues

import okasaki.Queue
import okasaki.queues.HoodMelvilleQueue._

object HoodMelvilleQueue {

  sealed trait RotationState[+E]

  object Idle extends RotationState[Nothing] {
    override def toString = s"Idle()"
  }

  case class Reversing[E](ok: Int, f: List[E], newf: List[E], r: List[E], newr: List[E]) extends RotationState[E]

  case class Appending[E](ok: Int, f: List[E], r: List[E]) extends RotationState[E]

  case class Done[E](newf: List[E]) extends RotationState[E]


  case class Repr[E](lenf: Int, f: List[E], state: RotationState[E], lenr: Int, r: List[E]) {
    def size = lenf + lenr
  }

}

class HoodMelvilleQueue[E] extends Queue[E, Repr[E]] {
  override def empty: Repr[E] = Repr(0, Nil, Idle, 0, Nil)

  override def isEmpty(q: Repr[E]): Boolean = q.lenf == 0

  def exec(state: RotationState[E]): RotationState[E] = state match {
    case Reversing(ok, x :: f, f1, y :: r, r1) => Reversing(ok + 1, f, x :: f1, r, y :: r1)
    case Reversing(ok, Nil, f1, y :: Nil, r1) => Appending(ok, f1, y :: r1)
    case Appending(0, _, r1) => Done(r1)
    case Appending(ok, x :: f1, r1) => Appending(ok - 1, f1, x :: r1)
    case _ => state
  }

  val execTwice = exec _ andThen exec

  def invalidate(state: RotationState[E]): RotationState[E] = state match {
    case Reversing(ok, f, f1, r, r1) => Reversing(ok - 1, f, f1, r, r1)
    case Appending(0, _, _ :: r1) => Done(r1)
    case Appending(ok, f1, r1) => Appending(ok - 1, f1, r1)
    case _ => state
  }

  def execute(q: Repr[E], step: RotationState[E] => RotationState[E]): Repr[E] = {
    step(q.state) match {
      case Done(newf) => q.copy(f = newf, state = Idle)
      case newstate => q.copy(state = newstate)
    }
  }

  def check(q: Repr[E]): Repr[E] =
    if (q.lenr <= q.lenf) execute(q, exec)
    else {
      val newstate = Reversing(0, q.f, Nil, q.r, Nil)
      execute(Repr(q.size, q.f, newstate, 0, Nil), execTwice)
    }

  override def snoc(q: Repr[E], x: E): Repr[E] = q match {
    case Repr(lenf, f, state, lenr, r) => check(Repr(lenf, f, state, lenr + 1, x :: r))
  }

  override def head(q: Repr[E]): E = q match {
    case Repr(_, Nil, _, _, _) => throw new IllegalStateException("head called on an empty queue")
    case Repr(_, x :: _, _, _, _) => x
  }

  override def tail(q: Repr[E]): Repr[E] = q match {
    case Repr(_, Nil, _, _, _) => throw new IllegalStateException("tail called on an empty queue")
    case Repr(lenf, _ :: f, state, lenr, r) =>
      check(Repr(lenf - 1, f, invalidate(state), lenr, r))
  }

}
