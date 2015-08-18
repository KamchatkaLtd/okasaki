package okasaki

import okasaki.RealTimeDeque.Repr
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RealTimeDequeSpec extends DequeSpec[Int, Repr[Int]] {
  override def elements: Arbitrary[Int] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def queue: Deque[Int, Repr[Int]] = new RealTimeDeque[Int](3)
}
