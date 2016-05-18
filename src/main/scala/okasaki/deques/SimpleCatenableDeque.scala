package okasaki.deques

import okasaki.deques.SimpleCatenableDeque._
import okasaki.misc.Susp
import okasaki.{CatenableDeque, Deque}

import scala.language.higherKinds

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object SimpleCatenableDeque {

  sealed trait Cat[E, Q[_]]

  case class Shallow[E, Q[_]](d: Q[E]) extends Cat[E, Q]

  case class Deep[E, Q[_]](f: Q[E], m: Susp[Cat[Q[E], Q]], r: Q[E]) extends Cat[E, Q]

  trait DequeLike[Q[_]] {
    def deque[E]: Deque[E, Q[E]]
  }

  def tooSmall[E, Q[_] : DequeLike](q: Q[E]) = {
    val d = implicitly[DequeLike[Q]].deque[E]
    d.isEmpty(q) || d.isEmpty(d.tail(q))
  }

  def dappendL[E, Q[_] : DequeLike](d1: Q[E], d2: Q[E]) = {
    val d = implicitly[DequeLike[Q]].deque[E]
    if (d.isEmpty(d1)) d2 else d.cons(d.head(d1), d2)
  }

  def dappendR[E, Q[_] : DequeLike](d1: Q[E], d2: Q[E]) = {
    val d = implicitly[DequeLike[Q]].deque[E]
    if (d.isEmpty(d2)) d1 else d.snoc(d1, d.head(d2))
  }

  def empty[E, Q[_] : DequeLike]: Cat[E, Q] = {
    val d = implicitly[DequeLike[Q]].deque[E]
    Shallow[E, Q](d.empty)
  }

  def isEmpty[E, Q[_] : DequeLike](cd: Cat[E, Q]): Boolean = cd match {
    case Shallow(ud) => implicitly[DequeLike[Q]].deque.isEmpty(ud)
    case _ => false
  }

  def cons[E, Q[_] : DequeLike](e: E, q: Cat[E, Q]): Cat[E, Q] = q match {
    case Shallow(ud) => Shallow[E, Q](implicitly[DequeLike[Q]].deque.cons(e, ud))
    case Deep(f, m, r) => Deep[E, Q](implicitly[DequeLike[Q]].deque.cons(e, f), m, r)
  }

  def head[E, Q[_] : DequeLike](q: Cat[E, Q]): E = q match {
    case Shallow(ud) => implicitly[DequeLike[Q]].deque.head(ud)
    case Deep(f, m, r) => implicitly[DequeLike[Q]].deque.head(f)
  }

  def tail[E, Q[_] : DequeLike](q: Cat[E, Q]): Cat[E, Q] = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => Shallow[E, Q](d.tail(ud))
      case Deep(f, m, r) =>
        val f1 = d.tail(f)
        if (!tooSmall(f1)) Deep[E, Q](f1, m, r)
        else if (isEmpty(m())) Shallow[E, Q](dappendL(f1, r))
        else {
          val fm: Cat[Q[E], Q] = m()
          Deep[E, Q](dappendL(f1, head(fm)), Susp(tail(fm)), r)
        }
    }
  }

  def snoc[E, Q[_] : DequeLike](q: Cat[E, Q], e: E): Cat[E, Q] = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => Shallow[E, Q](d.snoc(ud, e))
      case Deep(f, m, r) => Deep[E, Q](f, m, d.snoc(r, e))
    }
  }

  def last[E, Q[_] : DequeLike](q: Cat[E, Q]): E = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => d.last(ud)
      case Deep(f, m, r) => d.last(r)
    }
  }

  def init[E, Q[_] : DequeLike](q: Cat[E, Q]): Cat[E, Q] = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => Shallow[E, Q](d.init(ud))
      case Deep(f, m, r) =>
        val r1 = d.init(r)
        if (!tooSmall(r1)) Deep[E, Q](f, m, r1)
        else if (isEmpty(m())) Shallow[E, Q](dappendL(f, r1))
        else {
          val fm: Cat[Q[E], Q] = m()
          Deep[E, Q](f, Susp(init(fm)), dappendR(last(fm), r1))
        }
    }
  }

  def ++[E, Q[_] : DequeLike](a: Cat[E, Q], b: Cat[E, Q]): Cat[E, Q] = (a, b) match {
    case (Shallow(d1), Shallow(d2)) if tooSmall(d1) =>
      Shallow[E, Q](dappendL(d1, d2))
    case (Shallow(d1), Shallow(d2)) if tooSmall(d2) =>
      Shallow[E, Q](dappendR(d1, d2))
    case (Shallow(d1), Shallow(d2)) =>
      Deep[E, Q](d1, Susp(empty[Q[E], Q]), d2)
    case (Shallow(d), Deep(f, m, r)) if tooSmall(d) =>
      Deep[E, Q](dappendL(d, f), m, r)
    case (Shallow(d), Deep(f, m, r)) =>
      Deep[E, Q](d, Susp(cons(f, m())), r)
    case (Deep(f, m, r), Shallow(d)) if tooSmall(d) =>
      Deep[E, Q](f, m, dappendR(r, d))
    case (Deep(f, m, r), Shallow(d)) =>
      Deep[E, Q](f, Susp(snoc(m(), r)), d)
    case (Deep(f1, m1, r1), Deep(f2, m2, r2)) =>
      Deep[E, Q](f1, Susp(++(snoc(m1(), r1), cons(f2, m2()))), r2)
  }
}

trait SimpleCatenableDeque[E, Q[_]] extends CatenableDeque[E, Cat[E, Q]] {

  type D = Cat[E, Q]

  implicit def ud: DequeLike[Q]

  override def empty: D = SimpleCatenableDeque.empty[E, Q]

  override def isEmpty(cd: D): Boolean = SimpleCatenableDeque.isEmpty(cd)

  override def cons(e: E, q: D): D = SimpleCatenableDeque.cons(e, q)

  override def head(q: D): E = SimpleCatenableDeque.head(q)

  override def tail(q: D): D = SimpleCatenableDeque.tail(q)

  override def snoc(q: D, e: E): D = SimpleCatenableDeque.snoc(q, e)

  override def last(q: D): E = SimpleCatenableDeque.last(q)

  override def init(q: D): D = SimpleCatenableDeque.init(q)

  override def ++(a: D, b: D): D = SimpleCatenableDeque.++(a, b)
}
