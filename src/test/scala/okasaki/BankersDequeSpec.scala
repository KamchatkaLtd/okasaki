package okasaki

import okasaki.BankersDeque.Repr

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BankersDequeSpec extends DequeSpec[Int, Repr[Int]] with IntElements {
  override def queue: Deque[Int, Repr[Int]] = new BankersDeque[Int](3)
}
