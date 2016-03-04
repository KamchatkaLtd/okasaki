package okasaki.ra_lists

import okasaki.ra_list.SegmentedRedundantBinaryRandomAccessList
import okasaki.ra_list.SegmentedRedundantBinaryRandomAccessList._
import okasaki.{IntElements, RandomAccessListSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SegmentedRedundantBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SRList[Int], SegmentedRedundantBinaryRandomAccessList[Int]](new SegmentedRedundantBinaryRandomAccessList[Int])
  with IntElements
