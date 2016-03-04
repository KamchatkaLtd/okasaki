package okasaki.ra_lists

import okasaki.ra_list.RedundantZerolessBinaryRandomAccessList
import okasaki.ra_list.RedundantZerolessBinaryRandomAccessList._
import okasaki.{IntElements, RandomAccessListSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RedundantZerolessBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SRList[Int], RedundantZerolessBinaryRandomAccessList[Int]](new RedundantZerolessBinaryRandomAccessList[Int])
  with IntElements
