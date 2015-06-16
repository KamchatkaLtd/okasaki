package okasaki

import org.scalacheck.{Gen, Arbitrary}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class SetSpec[E, S] extends Specification with ScalaCheck {
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

      a.map(set.member(_, s)).reduce(_ && _) should beTrue
    }
  }

  implicit def manyElements: Arbitrary[Seq[E]] =
    Arbitrary(Gen.listOf(elements.arbitrary).filter(_.nonEmpty))

  def setFrom(es: Seq[E]): S = es.foldLeft(set.empty)((s, a) => set.insert(a, s))
}
