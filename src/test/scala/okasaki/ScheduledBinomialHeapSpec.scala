package okasaki

import okasaki.ScheduledBinomialHeap.Repr

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ScheduledBinomialHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = Repr[E]

  override val heap: ScheduledBinomialHeap[E] = new ScheduledBinomialHeap[E] {
    override def ord: Ordering[E] = Ordering.Int
  }
}
