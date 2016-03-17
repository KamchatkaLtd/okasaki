package okasaki.queues

import okasaki.Queue
import okasaki.queues.IndexedHoodMelvilleQueue._
import okasaki.ra_list.SkewBinaryRandomAccessList
import okasaki.ra_list.SkewBinaryRandomAccessList.SkewList

object IndexedHoodMelvilleQueue {

  sealed trait RotationState[+E]

  object Idle extends RotationState[Nothing] {
    override def toString = s"Idle()"
  }

  case class Reversing[E](ok: Int, f: SkewList[E], newf: SkewList[E], r: SkewList[E], newr: SkewList[E]) extends RotationState[E]

  case class Appending[E](ok: Int, f: SkewList[E], r: SkewList[E]) extends RotationState[E]

  case class Done[E](newf: SkewList[E]) extends RotationState[E]


  case class Repr[E](lenf: Int, f: SkewList[E], state: RotationState[E], lenr: Int, r: SkewList[E]) {
    def size = lenf + lenr
  }

}


class IndexedHoodMelvilleQueue[E] extends Queue[E, Repr[E]] {

  private val ral = new SkewBinaryRandomAccessList[E]()

  override def empty: Repr[E] = Repr(0, ral.empty, Idle, 0, ral.empty)

  override def isEmpty(q: Repr[E]): Boolean = q.lenf == 0

  def exec(state: RotationState[E]): RotationState[E] = state match {
    case Reversing(ok, f, f1, r, r1) if !ral.isEmpty(f) =>
      val x = ral.head(f)
      val y = ral.head(r)
      Reversing(ok + 1, ral.tail(f), ral.cons(f1, x), ral.tail(r), ral.cons(r1, y))
    case Reversing(ok, f, f1, r, r1) if ral.isEmpty(f) =>
      val y = ral.head(r)
      Appending(ok, f1, ral.cons(r1, y))
    case Appending(0, _, r1) =>
      Done(r1)
    case Appending(ok, f, r1) if !ral.isEmpty(f) =>
      val x = ral.head(f)
      val f1 = ral.tail(f)
      Appending(ok - 1, f1, ral.cons(r1, x))
    case _ => state
  }

  val execTwice = exec _ andThen exec

  def invalidate(state: RotationState[E]): RotationState[E] = state match {
    case Reversing(ok, f, f1, r, r1) => Reversing(ok - 1, f, f1, r, r1)
    case Appending(0, _, r1) => Done(ral.tail(r1))
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
      val newstate = Reversing(0, q.f, ral.empty, q.r, ral.empty)
      execute(Repr(q.size, q.f, newstate, 0, ral.empty), execTwice)
    }

  override def snoc(q: Repr[E], x: E): Repr[E] = q match {
    case Repr(lenf, f, state, lenr, r) => check(Repr(lenf, f, state, lenr + 1, ral.cons(r, x)))
  }

  override def head(q: Repr[E]): E = ral.head(q.f)

  override def tail(q: Repr[E]): Repr[E] = q match {
    case Repr(_, f, _, _, _) if ral.isEmpty(f) => throw new IllegalStateException("tail called on an empty queue")
    case Repr(lenf, f, state, lenr, r) =>
      check(Repr(lenf - 1, ral.tail(f), invalidate(state), lenr, r))
  }

}
