package okasaki

import okasaki.LazyBinomialHeap.BHeap

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LazyBinomialHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = BHeap[E]

  override val heap: LazyBinomialHeap[E] = new LazyBinomialHeap[E] {
    override def ord: Ordering[E] = Ordering.Int
  }
}
