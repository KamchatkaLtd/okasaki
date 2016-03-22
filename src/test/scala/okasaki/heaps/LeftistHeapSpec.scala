package okasaki.heaps

import okasaki.{HeapSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LeftistHeapSpec
  extends HeapSpec[Int, LeftistHeap[Int]]
  with IntElements {

  def empty = new LeftistHeap[Int]
}