package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class WeightBiasedLeftistHeapSpec
  extends HeapSpec[Int, WeightBiasedLeftistHeap[Int]]
  with IntElements {

  def empty = new WeightBiasedLeftistHeap[Int]
}