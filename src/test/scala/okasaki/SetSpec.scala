package okasaki

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class SetSpec[E] extends Specification with ScalaCheck {

  implicit def elements: Arbitrary[E]

  def set: Set[E]

  "An empty set" should {
    "contain no elements" ! prop { a: E =>
      set.empty.member(a) must beFalse
    }
  }

  "A non-empty set" should {
    "contain its elements" ! prop { a: Seq[E] =>
      val s = setFrom(a)

      a.forall(s.member) should beTrue
    }

    "not contain extra elements" ! prop { (a: Seq[E], e: E) =>
      val s = setFrom(a)

      s.member(e) === a.contains(e)
    }
  }

  implicit def manyElements: Arbitrary[Seq[E]] =
    Arbitrary(Gen.listOf(elements.arbitrary).filter(_.nonEmpty))

  private def setFrom(es: Seq[E]): Set[E] = es.foldLeft(set.empty)(_ insert _)
}
