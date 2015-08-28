package okasaki

import okasaki.RealTimeDeque.Repr

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RealTimeDequeSpec extends DequeSpec[Int, Repr[Int]] with IntElements {
  override def queue: Deque[Int, Repr[Int]] = new RealTimeDeque[Int](3)
}
