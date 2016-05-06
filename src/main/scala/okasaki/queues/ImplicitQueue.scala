package okasaki.queues

import okasaki.Queue
import okasaki.misc.Susp
import okasaki.queues.ImplicitQueue._

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object ImplicitQueue {

  sealed trait Digit[+E]

  case object Zero extends Digit[Nothing]

  case class One[E](x: E) extends Digit[E]

  case class Two[E](x: E, y: E) extends Digit[E]


  sealed trait Repr[E]

  case class Shallow[E](d: Digit[E]) extends Repr[E]

  case class Deep[E](l: Digit[E], m: Susp[Repr[(E, E)]], r: Digit[E]) extends Repr[E]


  def empty[E]: Repr[E] = Shallow(Zero)

  def isEmpty[E](q: Repr[E]) = q == Shallow(Zero)

  def snoc[E](q: Repr[E], e: E): Repr[E] = q match {
    case Shallow(Zero) => Shallow(One(e))
    case Shallow(One(x)) => Deep(Two(x, e), Susp(empty), Zero)
    case Deep(f, m, Zero) => Deep(f, m, One(e))
    case Deep(f, m, One(x)) => Deep(f, Susp(snoc(m(), (x, e))), Zero)
  }

  def head[E](q: Repr[E]): E = q match {
    case Shallow(Zero) => throw new IllegalStateException("empty queue")
    case Shallow(One(x)) => x
    case Deep(One(x), _, _) => x
    case Deep(Two(x, _), _, _) => x
  }

  def tail[E](q: Repr[E]): Repr[E] = q match {
    case Shallow(Zero) => throw new IllegalStateException("empty queue")
    case Shallow(One(x)) => empty
    case Deep(Two(_, y), m, r) => Deep(One(y), m, r)
    case Deep(One(_), Susp(queue), r) if isEmpty(queue) => Shallow(r)
    case Deep(One(_), Susp(queue), r) =>
      val (y, z) = head(queue)
      Deep(Two(y, z), Susp(tail(queue)), r)
  }
}

class ImplicitQueue[E] extends Queue[E, Repr[E]] {
  override def empty = ImplicitQueue.empty

  override def isEmpty(q: Repr[E]) = ImplicitQueue.isEmpty(q)

  override def snoc(q: Repr[E], e: E) = ImplicitQueue.snoc(q, e)

  override def head(q: Repr[E]): E = ImplicitQueue.head(q)

  override def tail(q: Repr[E]): Repr[E] = ImplicitQueue.tail(q)
}
