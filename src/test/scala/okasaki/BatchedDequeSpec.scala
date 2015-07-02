package okasaki

import org.scalacheck.{Arbitrary, Gen}

/**
  * Copyright (C) 2015 Kamchatka Ltd
  */
class BatchedDequeSpec extends DequeSpec[Int, (List[Int], List[Int])] {
   override def elements: Arbitrary[Int] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

   override def queue: Deque[Int, (List[Int], List[Int])] = new BatchedDeque[Int] {}
 }
