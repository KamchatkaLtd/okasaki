package okasaki

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait SetSpec[E, S] extends Specification with ScalaCheck {
  def set: Set[E, S]

  implicit def elements: Arbitrary[E]

  "An empty set" should {
    "contain no elements" ! prop { a: E =>
      set.member(a, set.empty) must beFalse
    }
  }

  "A non-empty set" should {
    "contain its elements" ! prop { a: Seq[E] =>
      val s = setFrom(a)

      a.forall(set.member(_, s)) should beTrue
    }

    "not contain extra elements" ! prop { (a: Seq[E], e: E) =>
      val s = setFrom(a)

      set.member(e, s) === a.contains(e)
    }
  }

  implicit def manyElements: Arbitrary[Seq[E]] =
    Arbitrary(Gen.listOf(elements.arbitrary).filter(_.nonEmpty))

  def setFrom(es: Seq[E]): S = es.foldRight(set.empty)(set.insert)
}
