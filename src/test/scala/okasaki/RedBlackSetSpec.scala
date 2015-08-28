package okasaki

import okasaki.RedBlackSet.RBTree

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

class RedBlackSetSpec extends SetSpec[Int, RBTree[Int]] with IntElements {
  override def set: RedBlackSet[Int] = new RedBlackSet[Int]()

  "fromOrdList" should {
    "contain its elements" ! prop { a: List[Int] =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      a.forall(set.member(_, s)) should beTrue
    }

    "be balanced" ! prop { (a: List[Int]) =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      set.isValid(s) should beTrue
    }

    "not contain extra elements" ! prop { (a: List[Int], e: Int) =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      set.member(e, s) === a.contains(e)
    }
  }

  "red-black set" should {
    "be balanced" ! prop { (a: Seq[Int]) =>
      val s = setFrom(a)

      set.isValid(s) should beTrue
    }
  }
}
