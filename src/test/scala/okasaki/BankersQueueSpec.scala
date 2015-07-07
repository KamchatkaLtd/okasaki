package okasaki

import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BankersQueueSpec extends QueueSpec[Int, (Int, Stream[Int], Int, Stream[Int])] {
  override def elements: Arbitrary[Int] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def queue: Queue[Int, (Int, Stream[Int], Int, Stream[Int])] = new BankersQueue[Int] {}
}
