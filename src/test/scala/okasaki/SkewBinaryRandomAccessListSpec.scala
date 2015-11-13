package okasaki

import okasaki.SkewBinaryRandomAccessList._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SkewBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SkewList[Int], SkewBinaryRandomAccessList[Int]](new SkewBinaryRandomAccessList[Int])
  with IntElements
