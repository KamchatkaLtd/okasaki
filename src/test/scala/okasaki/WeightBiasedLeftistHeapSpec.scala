package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class WeightBiasedLeftistHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = WeightBiasedLeftistHeap.Repr[E]

  override def heap: Heap[E, H] = new WeightBiasedLeftistHeap[Int]()
}
