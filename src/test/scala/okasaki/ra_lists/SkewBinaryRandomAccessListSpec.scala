package okasaki.ra_lists

import okasaki.ra_list.SkewBinaryRandomAccessList
import okasaki.ra_list.SkewBinaryRandomAccessList._
import okasaki.{IntElements, RandomAccessListSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SkewBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SkewList[Int], SkewBinaryRandomAccessList[Int]](new SkewBinaryRandomAccessList[Int])
  with IntElements
