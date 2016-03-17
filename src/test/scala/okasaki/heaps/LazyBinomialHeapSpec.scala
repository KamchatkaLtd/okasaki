package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LazyBinomialHeapSpec
  extends HeapSpec(new LazyBinomialHeap[Int])
  with IntElements