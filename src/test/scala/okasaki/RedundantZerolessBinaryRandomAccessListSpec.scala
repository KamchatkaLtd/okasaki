package okasaki

import okasaki.RedundantZerolessBinaryRandomAccessList._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RedundantZerolessBinaryRandomAccessListSpec
  extends RandomAccessListSpec[Int, SRList[Int], RedundantZerolessBinaryRandomAccessList[Int]](new RedundantZerolessBinaryRandomAccessList[Int])
  with IntElements
