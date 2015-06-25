package okasaki

import okasaki.RedBlackSet.RBTree
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

class RedBlackSetSpec extends SetSpec[Int, RBTree[Int]]{
  override def set: Set[Int, RBTree[Int]] = new RedBlackSet[Int]()

  implicit def elements: Arbitrary[Int] = Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))
}
