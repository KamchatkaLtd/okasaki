package okasaki.misc

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015-2019 Kamchatka Ltd
 */
class ExerciseSpec extends Specification with ScalaCheck {
  "Complete set" should {
    "Have size 2^d - 1" ! prop {
      d: Int =>
        Exercises.complete("element", d).size === (1L << d) - 1
    }
  }

  "Almost complete set" should {
    "Have requested size" ! prop {
      n: Int =>
        Exercises.almostComplete("element", n).size === n
    }
  }

  implicit val sizes: Arbitrary[Int] = Arbitrary(Gen.chooseNum(0, 30))
}
