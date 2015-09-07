package okasaki

import okasaki.BinaryRandomAccessList._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.choose

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, RList[Int], BinaryRandomAccessList[Int]](new BinaryRandomAccessList[Int])
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
