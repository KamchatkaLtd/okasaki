package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SkewBinomialHeapSpec
  extends HeapSpec[Int, SkewBinomialHeap[Int]]
  with IntElements {

  def empty = new SkewBinomialHeap[Int]
}