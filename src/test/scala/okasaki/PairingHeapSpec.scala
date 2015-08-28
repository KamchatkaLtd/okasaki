package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class PairingHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = PairingHeap.PHeap[Int]

  override def heap: Heap[E, H] = new PairingHeap[E]
}
