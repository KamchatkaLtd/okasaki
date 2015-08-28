package okasaki

import org.scalacheck.Arbitrary

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait IntElements {
  def elements: Arbitrary[Int] = Arbitrary.arbInt
}
