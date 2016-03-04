package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SplayHeapSpec
  extends HeapSpec(new SplayHeap[Int])
  with IntElements {

  "sort" should {
    "be identical to built-in" ! prop {
      (xs: List[Int]) =>
        SplayHeap.sort(xs) === xs.sorted
    }
  }
}
