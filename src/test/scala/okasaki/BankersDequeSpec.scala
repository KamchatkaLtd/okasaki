package okasaki

import okasaki.BankersDeque.Repr
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BankersDequeSpec extends DequeSpec[Int, Repr[Int]] {
  override def elements: Arbitrary[Int] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def queue: Deque[Int, Repr[Int]] = new BankersDeque[Int](3)
}
