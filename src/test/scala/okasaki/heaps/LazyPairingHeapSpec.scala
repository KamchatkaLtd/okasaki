package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LazyPairingHeapSpec
  extends HeapSpec(new LazyPairingHeap[Int])
  with IntElements
