package okasaki

import org.scalacheck.{Arbitrary, Gen}

import scala.language.implicitConversions

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
abstract class CatenableDequeSpec[E, Q](deque: CatenableDeque[E, Q]) extends DequeSpec(deque) {
  "A catenable deque" should {
    "Support concatenation (LL)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListL(a), fromListL(b)))
        res === (a ++ b)
    }
    "Support concatenation (LR)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListL(a), fromListR(b)))
        res === (a ++ b)
    }
    "Support concatenation (RL)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListR(a), fromListL(b)))
        res === (a ++ b)
    }
    "Support concatenation (RR)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListR(a), fromListR(b)))
        res === (a ++ b)
    }
    "Support concatenation L*" ! prop {
      (ls: List[List[E]]) =>
        val ds = ls map fromListL
        val res = ds.foldLeft(deque.empty)(deque.++)
        drain(res) === ls.flatten
    }
    "Support concatenation R*" ! prop {
      (ls: List[List[E]]) =>
        val ds = ls map fromListR
        val res = ds.foldRight(deque.empty)(deque.++)
        drain(res) === ls.flatten
    }
  }

  implicit def listOfLists[T](implicit data: Arbitrary[T]): Arbitrary[List[List[T]]] =
    Arbitrary(Gen.listOf(Gen.listOf(data.arbitrary)))

  def fromListR(xs: List[E]): Q =
    xs.foldRight(deque.empty)(deque.cons)

  def fromListL(xs: List[E]): Q =
    xs.foldLeft(deque.empty)(deque.snoc)
}
