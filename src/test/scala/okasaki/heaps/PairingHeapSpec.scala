package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class PairingHeapSpec
  extends HeapSpec(new PairingHeap[Int])
  with IntElements