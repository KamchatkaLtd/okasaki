package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class WeightBiasedLeftistHeapSpec
  extends HeapSpec(new WeightBiasedLeftistHeap[Int])
  with IntElements