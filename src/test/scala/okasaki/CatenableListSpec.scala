package okasaki

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.collection.immutable.Stream.iterate

/**
* Copyright (C) 2016 Kamchatka Ltd
*/
abstract class CatenableListSpec[E, CL](cl: CatenableList[E, CL]) extends Specification with ScalaCheck {
  implicit def elements: Arbitrary[E]

  "A catenable list" should {
    "Maintain the order (R)" ! prop {
        xs: List[E] =>
        val xs1 = drain(fromListR(xs))
        xs1 === xs
    }
    "Maintain the order (L)" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromListL(xs))
        xs1 === xs
    }
    "Support concatenation (LL)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(cl.++(fromListL(a), fromListL(b)))
        res === (a ++ b)
    }
    "Support concatenation (LR)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(cl.++(fromListL(a), fromListR(b)))
        res === (a ++ b)
    }
    "Support concatenation (RL)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(cl.++(fromListR(a), fromListL(b)))
        res === (a ++ b)
    }
    "Support concatenation (RR)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(cl.++(fromListR(a), fromListR(b)))
        res === (a ++ b)
    }
  }

  def fromListR(xs: List[E]): CL =
    xs.foldRight(cl.empty)(cl.cons)

  def fromListL(xs: List[E]): CL =
    xs.foldLeft(cl.empty)(cl.snoc)

  def drain(l: CL): List[E] =
    iterate(l)(cl.tail)
      .takeWhile(!cl.isEmpty(_))
      .map(cl.head)
      .toList
}
