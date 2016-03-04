package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BinomialHeapSpec
  extends HeapSpec(new BinomialHeap[Int])
  with IntElements
