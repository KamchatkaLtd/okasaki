package okasaki

import okasaki.SegmentedRedundantBinaryRandomAccessList._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SegmentedRedundantBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SRList[Int], SegmentedRedundantBinaryRandomAccessList[Int]](new SegmentedRedundantBinaryRandomAccessList[Int])
  with IntElements
