package okasaki

import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class HoodMelvilleQueueSpec extends QueueSpec[Int, HoodMelvilleQueue.Repr[Int]] {
  override def elements: Arbitrary[Int] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def queue: Queue[Int, HoodMelvilleQueue.Repr[Int]] = new HoodMelvilleQueue[Int]
}
