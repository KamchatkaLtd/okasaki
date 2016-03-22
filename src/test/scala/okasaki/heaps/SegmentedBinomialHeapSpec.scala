package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SegmentedBinomialHeapSpec
  extends HeapSpec[Int, SegmentedBinomialHeap[Int]]
  with IntElements {

  def empty = new SegmentedBinomialHeap[Int]

  "bug 1: sorting" should {
    "work" in {
      val a = List(3, 2, 1, 4, 0)
      val hh = fromList(a)

      println(hh)
      println(hh.deleteMin)
      println(hh.deleteMin.deleteMin)
      println(hh.deleteMin.deleteMin.deleteMin)

      hh.toList === a.sorted
    }
  }

  "bug 2: sorting" should {
    "work" in {
      val a = List(1, 2, 0)
      val hh = fromList(a)
      hh.toList === a.sorted
    }
  }

}
