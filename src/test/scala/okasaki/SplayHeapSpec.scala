package okasaki

import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SplayHeapSpec extends HeapSpec {
  override type E = Int

  override type H = SplayHeap.SHeap[Int]

  override implicit def elements: Arbitrary[E] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def heap: Heap[E, H] = new SplayHeap[E]

  "sort" should {
    "be identical to built-in" ! prop {
      (xs: List[E]) =>
        SplayHeap.sort(xs) === xs.sorted
    }
  }
}
