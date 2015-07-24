package okasaki

import okasaki.ScheduledBinomialHeap.Repr
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ScheduledBinomialHeapSpec extends HeapSpec {
  override type E = Int

  override type H = Repr[E]

  override implicit def elements: Arbitrary[E] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override val heap: ScheduledBinomialHeap[E] = new ScheduledBinomialHeap[E] {
    override def ord: Ordering[E] = Ordering.Int
  }
}
