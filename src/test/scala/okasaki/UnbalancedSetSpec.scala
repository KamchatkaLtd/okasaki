package okasaki

import org.scalacheck.{Gen, Arbitrary}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

class UnbalancedSetSpec extends SetSpec[Int, Tree[Int]]{
  implicit val order = new okasaki.Ordered[Int] {
    override def eq(a: Int, b: Int): Boolean = a == b

    override def leq(a: Int, b: Int): Boolean = a <= b

    override def lt(a: Int, b: Int): Boolean = a < b
  }

  override def set: Set[Int, Tree[Int]] = new UnbalancedSet[Int]

  implicit def elements: Arbitrary[Int] = Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))
}
