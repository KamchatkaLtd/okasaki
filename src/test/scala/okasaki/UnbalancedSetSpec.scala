package okasaki

import org.scalacheck.{Gen, Arbitrary}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

class UnbalancedSetSpec extends SetSpec[Int, BinaryTree[Int]]{
  override def set: Set[Int, BinaryTree[Int]] = new UnbalancedSet[Int]

  implicit def elements: Arbitrary[Int] = Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))
}
