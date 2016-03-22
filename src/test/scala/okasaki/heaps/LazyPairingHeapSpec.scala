package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LazyPairingHeapSpec
  extends HeapSpec[Int, LazyPairingHeap[Int]]
  with IntElements {

  def empty = new LazyPairingHeap[Int]
}
