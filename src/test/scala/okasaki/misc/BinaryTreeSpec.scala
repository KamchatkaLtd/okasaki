package okasaki.misc

import okasaki.misc.BinaryTree._
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
class BinaryTreeSpec extends Specification {
  "A binary tree" should {
    "Provide a nice toString" in {
      Empty.toString ===
        "E"
      SubBinaryTree(Empty, 1, Empty).toString ===
        "T(E,1,E)"
      SubBinaryTree(SubBinaryTree(Empty, 2, Empty), 1, Empty).toString ===
        "T(T(E,2,E),1,E)"
      SubBinaryTree(SubBinaryTree(Empty, 2, Empty), 1, SubBinaryTree(Empty, 3, Empty)).toString ===
        "T(T(E,2,E),1,T(E,3,E))"
    }
  }
}
