package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BinomialHeapSpec
  extends HeapSpec[Int, BinomialHeap[Int]]
  with IntElements {
  def empty = new BinomialHeap[Int]
}
