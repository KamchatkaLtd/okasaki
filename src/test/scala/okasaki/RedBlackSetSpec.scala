package okasaki

import okasaki.RedBlackSet.RBTree
import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

class RedBlackSetSpec extends SetSpec[Int, RBTree[Int]]{
  override def set: Set[Int, RBTree[Int]] = new RedBlackSet[Int]()

  implicit def elements: Arbitrary[Int] = Arbitrary(Gen.choose(Int.MinValue, Int.MaxValue))

  "fromOrdList" should {
    "contain its elements" ! prop { a: List[Int] =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      a.forall(set.member(_, s)) should beTrue
    }

    "not contain extra elements" ! prop { (a: List[Int], e: Int) =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      set.member(e, s) === a.contains(e)
    }
  }
}
