package okasaki.deques

import okasaki.Deque
import okasaki.deques.BankersDeque._

import scala.collection.immutable.Stream.{#::, Empty}

object BankersDeque {

  case class Repr[E](lenf: Int, f: Stream[E], lenr: Int, r: Stream[E]) {
    lazy val length = lenf + lenr
  }

}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BankersDeque[E](c: Int) extends Deque[E, Repr[E]] {
  override def empty: Repr[E] = Repr(0, Empty, 0, Empty)

  override def isEmpty: (Repr[E]) => Boolean = _.length == 0

  def check(q: Repr[E]): Repr[E] = q match {
    case Repr(lenf, f, lenr, r) if lenf > c * lenr + 1 =>
      val i = q.length / 2
      val j = q.length - i
      val (f1, f2) = f.splitAt(i)
      val r1 = r #::: f2.reverse
      Repr(i, f1, j, r1)
    case Repr(lenf, f, lenr, r) if lenr > c * lenf + 1 =>
      val j = q.length / 2
      val i = q.length - j
      val (r1, r2) = r.splitAt(j)
      val f1 = f #::: r2.reverse
      Repr(i, f1, j, r1)
    case _ => q
  }

  override def snoc: (Repr[E], E) => Repr[E] = {
    case (Repr(lenf, f, lenr, r), x) => check(Repr(lenf, f, lenr + 1, x #:: r))
  }

  override def tail: (Repr[E]) => Repr[E] = {
    case Repr(_, Empty, _, Empty) => throw new IllegalStateException("tail called on an empty deque")
    case Repr(_, Empty, _, _ #:: _) => empty
    case Repr(lenf, _ #:: f, lenr, r) => check(Repr(lenf - 1, f, lenr, r))
  }

  override def head: (Repr[E]) => E = {
    case Repr(_, Empty, _, Empty) => throw new IllegalStateException("head called on an empty deque")
    case Repr(_, Empty, _, x #:: _) => x
    case Repr(_, x #:: _, _, _) => x
  }

  override def cons: (Repr[E], E) => Repr[E] = {
    case (Repr(lenf, f, lenr, r), x) => check(Repr(lenf + 1, x #:: f, lenr, r))
  }

  override def init: (Repr[E]) => Repr[E] = {
    case Repr(_, Empty, _, Empty) => throw new IllegalStateException("init called on an empty deque")
    case Repr(_, _ #:: _, _, Empty) => empty
    case Repr(lenf, f, lenr, _ #:: r) => check(Repr(lenf, f, lenr - 1, r))
  }

  override def last: (Repr[E]) => E = {
    case Repr(_, Empty, _, Empty) => throw new IllegalStateException("last called on an empty deque")
    case Repr(_, x #:: _, _, Empty) => x
    case Repr(_, _, _, x #:: _) => x
  }
}
