package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LazyBinomialHeapSpec
  extends HeapSpec[Int, LazyBinomialHeap[Int]]
  with IntElements {

  def empty = new LazyBinomialHeap[Int]
}