package okasaki

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait HeapSpec extends Specification with ScalaCheck {
  type E

  type H

  def heap: Heap[E, H]

  implicit def elements: Arbitrary[E]

  implicit def heaps: Arbitrary[H] = Arbitrary(Gen.nonEmptyListOf(elements.arbitrary).map(fromList))

  "heap" should {
    "provide an empty heap" in {
      heap.isEmpty(heap.empty)
    }
  }

  "empty heap" should {
    "allow insertion" ! prop {
      e: E =>
        val h = heap.empty
        heap.findMin(heap.insert(e, h)) === e
    }
  }

  "any heap" should {
    "allow insertion" ! prop {
      (a: E, h: H) =>
        val hwitha = heap.insert(a, h)
        heap.ord.lteq(heap.findMin(hwitha), a) should beTrue
    }
  }

  "merge" should {
    "preserve min" ! prop {
      (h1: H, h2: H) =>
        val h = heap.merge(h1, h2)
        heap.ord.lteq(heap.findMin(h), heap.findMin(h1)) should beTrue
    }
  }

  "sorting" should {
    "be natural" ! prop {
      (a: List[E], h: H) =>
        val hh = fromList(a)
        drain(hh) === a.sorted(heap.ord)
    }
  }

  def fromList(a: List[E]): H =
    a.foldRight(heap.empty)(heap.insert)

  def drain(h: H): List[E] =
    if (heap.isEmpty(h)) Nil
    else heap.findMin(h) :: drain(heap.deleteMin(h))
}
