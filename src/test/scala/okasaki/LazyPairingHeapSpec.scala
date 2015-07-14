package okasaki

import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LazyPairingHeapSpec extends HeapSpec {
  override type E = Int

  override type H = LazyPairingHeap.Repr[Int]

  override implicit def elements: Arbitrary[E] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def heap: Heap[E, H] = new LazyPairingHeap[E]
}
