package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

class UnbalancedSetSpec extends SetSpec[Int, BinaryTree[Int]] with IntElements {
  override def set: Set[Int, BinaryTree[Int]] = new UnbalancedSet[Int]
}
