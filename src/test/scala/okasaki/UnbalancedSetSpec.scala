package okasaki

import org.scalacheck.{Gen, Arbitrary}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

class UnbalancedSetSpec extends SetSpec[Int, Tree[Int]]{
  override def set: Set[Int, Tree[Int]] = new UnbalancedSet[Int]

  implicit def elements: Arbitrary[Int] = Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))
}
