package okasaki.sets

import okasaki.misc.BinaryTree
import okasaki.{IntElements, SetSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class UnbalancedSetSpec
  extends SetSpec[Int, BinaryTree[Int], UnbalancedSet[Int]](new UnbalancedSet[Int])
  with IntElements