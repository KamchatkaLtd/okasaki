package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ScheduledBinomialHeapSpec
  extends HeapSpec(new ScheduledBinomialHeap[Int])
  with IntElements