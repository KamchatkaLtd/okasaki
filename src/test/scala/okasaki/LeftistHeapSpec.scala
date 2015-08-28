package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LeftistHeapSpec
  extends HeapSpec(new LeftistHeap[Int])
  with IntElements