package okasaki

import org.scalacheck.{Gen, Arbitrary}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BatchedQueueSpec extends QueueSpec[Int, (List[Int], List[Int])] {
  override def elements: Arbitrary[Int] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def queue: Queue[Int, (List[Int], List[Int])] = new BatchedQueue[Int] {}
}
