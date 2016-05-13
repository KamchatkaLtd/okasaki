package okasaki.deques

import okasaki.Deque
import okasaki.deques.RealTimeDeque._

import scala.collection.immutable.Stream.{#::, Empty}

object RealTimeDeque {

  case class Repr[E](lenf: Int, f: Stream[E], sf: Stream[E], lenr: Int, r: Stream[E], sr: Stream[E]) {
    lazy val length = lenf + lenr
  }

}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RealTimeDeque[E](c: Int) extends Deque[E, Repr[E]] {
  override def empty: Repr[E] = Repr(0, Empty, Empty, 0, Empty, Empty)

  override def isEmpty(q: Repr[E]): Boolean = q.length == 0

  def rotateDrop(f: Stream[E], j: Int, r: Stream[E]): Stream[E] =
    if (j < c) rotateRev(f, r drop j, Empty)
    else {
      val x #:: f1 = f
      x #:: rotateDrop(f1, j - c, r drop c)
    }

  def rotateRev(f: Stream[E], r: Stream[E], a: Stream[E]): Stream[E] =
    if (f.isEmpty) r.reverse ++ a
    else f.head #:: {
      val (r1, r2) = r splitAt c
      rotateRev(f.tail, r2, r1.reverse ++ a)
    }

  def exec1(s: Stream[E]): Stream[E] = if (s.nonEmpty) s.tail else s

  def exec2(s: Stream[E]): Stream[E] = exec1(exec1(s))

  def check(q: Repr[E]): Repr[E] = q match {
    case Repr(lenf, f, sf, lenr, r, sr) if lenf > c * lenr + 1 =>
      val i = q.length / 2
      val j = q.length - i
      val f1 = f take i
      val r1 = rotateDrop(r, i, f)
      Repr(i, f1, f1, j, r1, r1)
    case Repr(lenf, f, sf, lenr, r, sr) if lenr > c * lenf + 1 =>
      val j = q.length / 2
      val i = q.length - j
      val r1 = r take j
      val f1 = rotateDrop(f, j, r)
      Repr(i, f1, f1, j, r1, r1)
    case _ => q
  }

  override def snoc(q: Repr[E], x: E): Repr[E] = q match {
    case Repr(lenf, f, sf, lenr, r, sr) => check(Repr(lenf, f, exec1(sf), lenr + 1, x #:: r, exec1(sr)))
  }

  override def tail(q: Repr[E]): Repr[E] = q match {
    case Repr(_, Empty, _, _, Empty, _) => throw new IllegalStateException("tail called on an empty deque")
    case Repr(_, Empty, _, _, _ #:: _, _) => empty
    case Repr(lenf, _ #:: f, sf, lenr, r, sr) => check(Repr(lenf - 1, f, exec2(sf), lenr, r, exec2(sr)))
  }

  override def head(q: Repr[E]): E = q match {
    case Repr(_, Empty, _, _, Empty, _) => throw new IllegalStateException("head called on an empty deque")
    case Repr(_, Empty, _, _, x #:: _, _) => x
    case Repr(_, x #:: _, _, _, _, _) => x
  }

  override def cons(x: E, q: Repr[E]): Repr[E] = q match {
    case Repr(lenf, f, sf, lenr, r, sr) => check(Repr(lenf + 1, x #:: f, exec1(sf), lenr, r, exec1(sr)))
  }

  override def init(q: Repr[E]): Repr[E] = q match {
    case Repr(_, Empty, _, _, Empty, _) => throw new IllegalStateException("init called on an empty deque")
    case Repr(_, _ #:: _, _, _, Empty, _) => empty
    case Repr(lenf, f, sf, lenr, _ #:: r, sr) => check(Repr(lenf, f, exec2(sf), lenr - 1, r, exec2(sr)))
  }

  override def last(q: Repr[E]): E = q match {
    case Repr(_, Empty, _, _, Empty, _) => throw new IllegalStateException("last called on an empty deque")
    case Repr(_, x #:: _, _, _, Empty, _) => x
    case Repr(_, _, _, _, x #:: _, _) => x
  }
}
