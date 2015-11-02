package okasaki

import okasaki.SegmentedBinomialHeap.SBHeap

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SegmentedBinomialHeapSpec
  extends HeapSpec[Int, SBHeap[Int]](new SegmentedBinomialHeap[Int])
  with IntElements {

  "bug 1: sorting" should {
    "work" in {
      val a = List(3, 2, 1, 4, 0)
      val hh = fromList(a)

      println(hh)
      println(heap.deleteMin(hh))
      println(heap.deleteMin(heap.deleteMin(hh)))
      println(heap.deleteMin(heap.deleteMin(heap.deleteMin(hh))))

      drain(hh) === a.sorted
    }
  }

  "bug 2: sorting" should {
    "work" in {
      val a = List(1, 2, 0)
      val hh = fromList(a)
      drain(hh) === a.sorted
    }
  }

}
