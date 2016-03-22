package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class PairingHeapSpec
  extends HeapSpec[Int, PairingHeap[Int]]
  with IntElements {

  def empty = new PairingHeap[Int]
}
