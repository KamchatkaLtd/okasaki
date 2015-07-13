package okasaki

import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class PhysicistQueueSpec extends QueueSpec[Int, PhysicistQueue.Repr[Int]] {
  override def elements: Arbitrary[Int] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def queue: Queue[Int, PhysicistQueue.Repr[Int]] = new PhysicistQueue[Int] {}
}
