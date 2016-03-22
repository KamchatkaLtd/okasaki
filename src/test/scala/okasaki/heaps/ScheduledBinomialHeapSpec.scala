package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ScheduledBinomialHeapSpec
  extends HeapSpec[Int, ScheduledBinomialHeap[Int]]
  with IntElements {

  def empty = new ScheduledBinomialHeap[Int]
}