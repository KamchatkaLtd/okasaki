package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SkewBinomialHeapSpec
  extends HeapSpec(new SkewBinomialHeap[Int])
  with IntElements