package okasaki.ra_list

import okasaki.RandomAccessList
import okasaki.RandomAccessList.Subscript
import okasaki.ra_list.AltBinaryRandomAccessList._

object AltBinaryRandomAccessList {

  sealed trait RList[+E]

  object Empty extends RList[Nothing] {
    override def toString = "Nil"
  }

  case class Zero[E](rest: RList[(E, E)]) extends RList[E]

  case class One[E](e: E, rest: RList[(E, E)]) extends RList[E]

  def repeat[E](n: Int, x: E): RList[E] = {
    def repeat1[E1](m: Int, xx: E1): RList[E1] = {
      if (m == 0) Empty
      else {
        val rest = repeat1(m / 2, (xx, xx))
        if (m % 2 == 1) One(xx, rest) else Zero(rest)
      }
    }

    repeat1(n, x)
  }

  def cons[E](l: RList[E], e: E): RList[E] = (l, e) match {
    case (Empty, x) => One(x, Empty)
    case (Zero(rest), x) => One(x, rest)
    case (One(y, rest), x) =>
      Zero(cons(rest, (x, y)))
  }

  def uncons[E](l: RList[E]): (E, RList[E]) = l match {
    case Empty => throw new Subscript
    case One(x, Empty) => (x, Empty)
    case One(x, rest) => (x, Zero(rest))
    case Zero(rest) =>
      val ((x, y), xs) = uncons(rest)
      (x, One(y, xs))
  }

  def lookup[E](i: Int, l: RList[E]): E = (i, l) match {
    case (_, Empty) => throw Subscript()
    case (0, One(x, _)) => x
    case (_, One(_, rest)) => lookup(i - 1, Zero(rest))
    case (_, Zero(rest)) =>
      val (x, y) = lookup(i / 2, rest)
      if (i % 2 == 0) x else y
  }

  def fupdate[E](f: E => E, i: Int, l: RList[E]): RList[E] = (i, l) match {
    case (_, Empty) => throw Subscript()
    case (0, One(x, rest)) => One(f(x), rest)
    case (_, One(x, rest)) => cons(fupdate(f, i - 1, Zero(rest)), x)
    case (_, Zero(rest)) =>
      val f1 =
        if (i % 2 == 0) (e: (E, E)) => (f(e._1), e._2)
        else (e: (E, E)) => (e._1, f(e._2))
      Zero(fupdate(f1, i / 2, rest))
  }
}

class AltBinaryRandomAccessList[E] extends RandomAccessList[E, RList[E]] {
  override def empty: RList[E] = Empty

  override def isEmpty: (RList[E]) => Boolean = _ == Empty

  override def cons: (RList[E], E) => RList[E] = AltBinaryRandomAccessList.cons

  override def head: (RList[E]) => E = uncons(_)._1

  override def tail: (RList[E]) => RList[E] = uncons(_)._2

  override def lookup: (Int, RList[E]) => E = AltBinaryRandomAccessList.lookup

  override def update: (Int, E, RList[E]) => RList[E] = (i, y, xs) => fupdate((_: E) => y, i, xs)
}
