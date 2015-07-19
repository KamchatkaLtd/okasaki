package okasaki

import org.scalacheck.{Gen, Arbitrary}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LeftistHeapSpec extends HeapSpec {
  override type E = Int

  override type H = LeftistHeap.Repr[E]

  override implicit def elements: Arbitrary[E] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def heap: Heap[E, H] = new LeftistHeap[Int]()
}
