package okasaki

import okasaki.OutputRestrictedDeque.{ForQueue, Repr}
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class OutputRestrictedDequeFromQueueSpec
  extends OutputRestrictedDequeSpec[Int, Repr[Int, HoodMelvilleQueue.Repr[Int]]] {
  override def elements = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def queue = new ForQueue(new HoodMelvilleQueue[Int])
}
