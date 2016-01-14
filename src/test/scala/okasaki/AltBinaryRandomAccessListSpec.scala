package okasaki

import okasaki.AltBinaryRandomAccessList._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.choose

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class AltBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, RList[Int], AltBinaryRandomAccessList[Int]](new AltBinaryRandomAccessList[Int])
  with IntElements {

  "repeat(n)" should {
    "support any non-negative n" ! prop {
      (a: Int, n: (List[Int], Int)) =>
        val index = n._2
        drain(repeat(index, a)) === List.iterate(a, index)(identity)
    }
  }


  implicit def arrayWithIndex: Arbitrary[(List[Int], Int)] = Arbitrary(
    for {
      xs <- arbitrary[List[Int]]
      n <- choose(0, xs.size)
    } yield (xs, n)
  )
}
