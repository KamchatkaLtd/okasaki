package okasaki.maps

import okasaki.{FiniteMap, FiniteMapSpec}
import org.scalacheck.Arbitrary

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
class UnbalancedMapSpec extends FiniteMapSpec[Int, String] {
  override implicit def keys: Arbitrary[Int] = Arbitrary.arbInt

  override implicit def elements: Arbitrary[String] = Arbitrary.arbString

  override def map: FiniteMap[Int, String] = new UnbalancedMap[Int, String]()
}
