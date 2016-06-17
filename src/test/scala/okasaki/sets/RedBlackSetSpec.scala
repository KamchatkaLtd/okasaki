package okasaki.sets

import okasaki.sets.RedBlackSet.RBTree
import okasaki.{IntElements, SetSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RedBlackSetSpec
  extends SetSpec[Int, RBTree[Int]]()
  with IntElements {

  import okasaki.Set._
  implicit val set = new RedBlackSet[Int]

  "fromOrdList" should {
    "contain its elements" ! prop { a: List[Int] =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      a.forall(s.member(_)) should beTrue
    }

    "be balanced" ! prop { (a: List[Int]) =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      RedBlackSet.isValid(s) should beTrue
    }

    "not contain extra elements" ! prop { (a: List[Int], e: Int) =>
      val s = RedBlackSet.fromOrdList(a.sorted)

      s.member(e) === a.contains(e)
    }
  }

  "red-black set" should {
    "be balanced" ! prop { (a: Seq[Int]) =>
      val s = RedBlackSet.fromOrdList(a.sorted.toList)

      RedBlackSet.isValid(s) should beTrue
    }
  }
}
