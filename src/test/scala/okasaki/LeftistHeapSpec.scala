package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LeftistHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = LeftistHeap.Repr[E]

  override def heap: Heap[E, H] = new LeftistHeap[Int]()
}
