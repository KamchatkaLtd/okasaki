package okasaki.ra_lists

import okasaki.ra_list.ZerolessBinaryRandomAccessList
import okasaki.ra_list.ZerolessBinaryRandomAccessList._
import okasaki.{IntElements, RandomAccessListSpec}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.choose

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ZerolessBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SRList[Int], ZerolessBinaryRandomAccessList[Int]](new ZerolessBinaryRandomAccessList[Int])
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
