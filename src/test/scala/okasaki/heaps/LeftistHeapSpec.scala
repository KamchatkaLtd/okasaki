package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LeftistHeapSpec
  extends HeapSpec(new LeftistHeap[Int])
  with IntElements