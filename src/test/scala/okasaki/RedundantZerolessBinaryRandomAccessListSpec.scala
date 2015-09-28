package okasaki

import okasaki.RedundantZerolessBinaryRandomAccessList._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.choose

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RedundantZerolessBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SRList[Int], RedundantZerolessBinaryRandomAccessList[Int]](new RedundantZerolessBinaryRandomAccessList[Int])
  with IntElements {

  "Binary random access list" should {
    "allow drop(n)" ! prop {
      in: (List[Int], Int) => {
        val (xs, n) = in
        val l = fromList(xs)

        drain(list.drop(n, l)) === xs.drop(n)
      }
    }
  }


  implicit def arrayWithIndex: Arbitrary[(List[Int], Int)] = Arbitrary(
    for {
      xs <- arbitrary[List[Int]]
      n <- choose(0, xs.size)
    } yield (xs, n)
  )
}
