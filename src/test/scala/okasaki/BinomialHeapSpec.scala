package okasaki

import okasaki.BinomialHeap.BHeap
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BinomialHeapSpec extends HeapSpec {
  override type E = Int

  override type H = BHeap[E]

  override implicit def elements: Arbitrary[E] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override val heap: BinomialHeap[E] = new BinomialHeap[E] {
    override def ord: Ordering[E] = Ordering.Int
  }
}
