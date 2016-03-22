package okasaki

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class HeapSpec[E, H <: Heap[E, H]] extends Specification with ScalaCheck {

  implicit def elements: Arbitrary[E]

  implicit def heaps: Arbitrary[H] = Arbitrary(Gen.nonEmptyListOf(elements.arbitrary).map(fromList))

  def empty: H

  lazy val ord: Ordering[E] = empty.ord

  "heap" should {
    "provide an empty heap" in {
      empty.isEmpty
    }
  }

  "empty heap" should {
    "allow insertion" ! prop {
      e: E =>
        val h = empty
        h.insert(e).findMin === e
    }

    "be empty for find" ! prop {
      e: E =>
        val h = empty
        h.findMin should throwAn[IllegalStateException]
    }

    "be empty for delete" ! prop {
      e: E =>
        val h = empty
        h.deleteMin should throwAn[IllegalStateException]
    }
  }

  "any heap" should {
    "allow insertion" ! prop {
      (a: E, h: H) =>
        val hwitha = h.insert(a)
        ord.lteq(hwitha.findMin, a) should beTrue
    }
  }

  "merge" should {
    "preserve min" ! prop {
      (h1: H, h2: H) =>
        val h = h1.merge(h2)
        ord.lteq(h.findMin, h2.findMin) should beTrue
        ord.lteq(h.findMin, h1.findMin) should beTrue
    }
  }

  "sorting" should {
    "be natural" ! prop {
      a: List[E] =>
        val hh = fromList(a)
        hh.toList === a.sorted(ord)
    }
  }

  def fromList(a: List[E]): H =
    a.foldLeft(empty)(_ insert _)
}
