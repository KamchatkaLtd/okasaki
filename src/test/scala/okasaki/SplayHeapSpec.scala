package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SplayHeapSpec extends HeapSpec with IntElements {
  override type E = Int

  override type H = SplayHeap.SHeap[Int]

  override def heap: Heap[E, H] = new SplayHeap[E]

  "sort" should {
    "be identical to built-in" ! prop {
      (xs: List[E]) =>
        SplayHeap.sort(xs) === xs.sorted
    }
  }
}
