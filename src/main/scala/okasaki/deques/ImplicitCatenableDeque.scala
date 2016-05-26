package okasaki.deques

import okasaki.deques.ImplicitCatenableDeque._
import okasaki.misc.Susp
import okasaki.{CatenableDeque, Deque}

import scala.language.higherKinds

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object ImplicitCatenableDeque {

  sealed trait Cat[E, Q[_]]

  case class Shallow[E, Q[_]](d: Q[E]) extends Cat[E, Q]

  case class Deep[E, Q[_]](f: Q[E],
                           a: Susp[Cat[CmpdElem[E, Q], Q]],
                           m: Q[E],
                           b: Susp[Cat[CmpdElem[E, Q], Q]],
                           r: Q[E]) extends Cat[E, Q]

  sealed trait CmpdElem[E, Q[_]]

  case class Simple[E, Q[_]](d: Q[E]) extends CmpdElem[E, Q]

  case class Comp[E, Q[_]](f: Q[E], m: Susp[Cat[CmpdElem[E, Q], Q]], r: Q[E]) extends CmpdElem[E, Q]

  trait DequeLike[Q[_]] {
    def deque[E]: Deque[E, Q[E]]
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
    case Deep(f, a, m, b, r) => Deep[E, Q](implicitly[DequeLike[Q]].deque.cons(e, f), a, m, b, r)
  }

  def head[E, Q[_] : DequeLike](q: Cat[E, Q]): E = q match {
    case Shallow(ud) => implicitly[DequeLike[Q]].deque.head(ud)
    case Deep(f, a, m, b, r) => implicitly[DequeLike[Q]].deque.head(f)
  }

  def snoc[E, Q[_] : DequeLike](q: Cat[E, Q], e: E): Cat[E, Q] = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => Shallow[E, Q](d.snoc(ud, e))
      case Deep(f, a, m, b, r) => Deep[E, Q](f, a, m, b, d.snoc(r, e))
    }
  }

  def last[E, Q[_] : DequeLike](q: Cat[E, Q]): E = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => d.last(ud)
      case Deep(f, a, m, b, r) => d.last(r)
    }
  }

  def shorterThan[E, Q[_] : DequeLike](n: Int, q: Q[E]): Boolean = {
    val d = implicitly[DequeLike[Q]].deque[E]

    (n >= 0) && (d.isEmpty(q) || shorterThan(n - 1, d.tail(q)))
  }

  def share[E, Q[_] : DequeLike](f: Q[E], r: Q[E]): (Q[E], Q[E], Q[E]) = {
    val d = implicitly[DequeLike[Q]].deque[E]
    val m = d.cons(d.last(f), d.cons(d.head(r), d.empty))
    (d.init(f), m, d.tail(r))
  }

  def dappendL[E, Q[_] : DequeLike](d1: Q[E], d2: Q[E]): Q[E] = {
    val d = implicitly[DequeLike[Q]].deque[E]
    if (d.isEmpty(d1)) d2
    else dappendL(d.init(d1), d.cons(d.last(d1), d2))
  }

  def dappendR[E, Q[_] : DequeLike](d1: Q[E], d2: Q[E]): Q[E] = {
    val d = implicitly[DequeLike[Q]].deque[E]
    if (d.isEmpty(d2)) d1
    else dappendR(d.snoc(d1, d.head(d2)), d.tail(d2))
  }

  def ++[E, Q[_] : DequeLike](q1: Cat[E, Q], q2: Cat[E, Q]): Cat[E, Q] = (q1, q2) match {
    case (Shallow(d1), Shallow(d2)) if shorterThan(4, d1) =>
      Shallow[E, Q](dappendL(d1, d2))
    case (Shallow(d1), Shallow(d2)) if shorterThan(4, d2) =>
      Shallow[E, Q](dappendR(d1, d2))
    case (Shallow(d1), Shallow(d2)) =>
      val (f, m, r) = share(d1, d2)
      Deep[E, Q](f, Susp(empty[CmpdElem[E, Q], Q]), m, Susp(empty[CmpdElem[E, Q], Q]), r)
    case (Shallow(d), Deep(f, a, m, b, r)) if shorterThan(4, d) =>
      Deep[E, Q](dappendL(d, f), a, m, b, r)
    case (Shallow(d), Deep(f, a, m, b, r)) =>
      Deep[E, Q](d, Susp(cons(Simple[E, Q](f), a())), m, b, r)
    case (Deep(f, a, m, b, r), Shallow(d)) if shorterThan(4, d) =>
      Deep[E, Q](f, a, m, b, dappendR(r, d))
    case (Deep(f, a, m, b, r), Shallow(d)) =>
      Deep[E, Q](f, a, m, Susp(snoc(b(), Simple[E, Q](r))), d)
    case (Deep(f1, a1, m1, b1, r1), Deep(f2, a2, m2, b2, r2)) =>
      val (r11, m, f21) = share(r1, f2)
      val a11 = Susp(snoc(a1(), Comp[E, Q](m1, b1, r11)))
      val b21 = Susp(cons(Comp[E, Q](f21, a1, m2), b2()))
      Deep[E, Q](f1, a11, m, b21, r2)
  }

  def replaceHead[E, Q[_] : DequeLike](x: E, q: Cat[E, Q]) = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(d1) => Shallow(d.cons(x, d.tail(d1)))
      case dq@Deep(f, _, _, _, _) => dq.copy(f = d.cons(x, d.tail(f)))
    }
  }

  def tail[E, Q[_] : DequeLike](q: Cat[E, Q]): Cat[E, Q] = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => Shallow[E, Q](d.tail(ud))
      case Deep(f, a, m, b, r) if !shorterThan(4, f) =>
        Deep[E, Q](d.tail(f), a, m, b, r)
      case Deep(f, a, m, b, r) if !isEmpty(a()) =>
        head(a()) match {
          case Simple(d1) =>
            val f1 = dappendL(d.tail(f), d1)
            Deep[E, Q](f1, Susp(tail(a())), m, b, r)
          case Comp(f1, c1, r1) =>
            val f11 = dappendL(d.tail(f), f1)
            val a11 = Susp(++(c1(), replaceHead(Simple(r1), a())))
            Deep[E, Q](f11, a11, m, b, r)
        }
      case Deep(f, a, m, b, r) if !isEmpty(b()) =>
        head(b()) match {
          case Simple(d1) =>
            val f1 = dappendL(d.tail(f), m)
            Deep[E, Q](f1, Susp(empty[CmpdElem[E, Q], Q]), d1, Susp(tail(b())), r)
          case Comp(f1, c1, r1) =>
            val f11 = dappendL(d.tail(f), m)
            val a11 = Susp(cons(Simple[E, Q](f1), c1()))
            Deep[E, Q](f11, a11, r1, Susp(tail(b())), r)
        }
      case Deep(f, a, m, b, r) =>
        ++(Shallow[E, Q](dappendL(d.tail(f), m)), Shallow(r))
    }
  }

  def replaceTail[E, Q[_] : DequeLike](q: Cat[E, Q], x: E) = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(d1) => Shallow(d.snoc(d.init(d1), x))
      case dq@Deep(_, _, _, _, r) => dq.copy(r = d.snoc(d.init(r), x))
    }
  }

  def init[E, Q[_] : DequeLike](q: Cat[E, Q]): Cat[E, Q] = {
    val d: Deque[E, Q[E]] = implicitly[DequeLike[Q]].deque
    q match {
      case Shallow(ud) => Shallow[E, Q](d.init(ud))
      case Deep(f, a, m, b, r) if !shorterThan(4, r) =>
        Deep[E, Q](f, a, m, b, d.init(r))
      case Deep(f, a, m, b, r) if !isEmpty(b()) =>
        last(b()) match {
          case Simple(d1) =>
            val r1 = dappendR(d1, d.init(r))
            Deep[E, Q](f, a, m, Susp(init(a())), r1)
          case Comp(f1, c1, r1) =>
            val r11 = dappendR(r1, d.init(r))
            val b11 = Susp(++(replaceTail(b(), Simple(f1)), c1()))
            Deep[E, Q](f, a, m, b11, r11)
        }
      case Deep(f, a, m, b, r) if !isEmpty(a()) =>
        last(a()) match {
          case Simple(d1) =>
            val r1 = dappendR(m, d.init(r))
            Deep[E, Q](f, Susp(init(a())), d1, Susp(empty[CmpdElem[E, Q], Q]), r1)
          case Comp(f1, c1, r1) =>
            val r11 = dappendR(m, d.init(r))
            val b11 = Susp(snoc(c1(), Simple[E, Q](r1)))
            Deep[E, Q](f, Susp(init(a())), f1, b11, r11)
        }
      case Deep(f, a, m, b, r) =>
        ++(Shallow(f), Shallow[E, Q](dappendR(m, d.init(r))))
    }
  }
}

trait ImplicitCatenableDeque[E, Q[_]] extends CatenableDeque[E, Cat[E, Q]] {

  type D = Cat[E, Q]

  implicit def ud: DequeLike[Q]

  override def empty: D = ImplicitCatenableDeque.empty[E, Q]

  override def isEmpty(cd: D): Boolean = ImplicitCatenableDeque.isEmpty(cd)

  override def cons(e: E, q: D): D = ImplicitCatenableDeque.cons(e, q)

  override def head(q: D): E = ImplicitCatenableDeque.head(q)

  override def tail(q: D): D = ImplicitCatenableDeque.tail(q)

  override def snoc(q: D, e: E): D = ImplicitCatenableDeque.snoc(q, e)

  override def last(q: D): E = ImplicitCatenableDeque.last(q)

  override def init(q: D): D = ImplicitCatenableDeque.init(q)

  override def ++(a: D, b: D): D = ImplicitCatenableDeque.++(a, b)
}
