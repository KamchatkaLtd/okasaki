package okasaki

import okasaki.BinomialHeap.BHeap

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BinomialHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = BHeap[E]

  override val heap: BinomialHeap[E] = new BinomialHeap[E] {
    override def ord: Ordering[E] = Ordering.Int
  }
}
