package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LazyPairingHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = LazyPairingHeap.Repr[Int]

  override def heap: Heap[E, H] = new LazyPairingHeap[E]
}
