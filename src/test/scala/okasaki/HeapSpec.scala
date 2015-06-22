package okasaki

import org.scalacheck.Arbitrary
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

}
