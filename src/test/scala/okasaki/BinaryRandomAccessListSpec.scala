package okasaki

import okasaki.BinaryRandomAccessList.RList

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BinaryRandomAccessListSpec extends RandomAccessListSpec[Int, RList[Int]] with IntElements {
  override def list: RandomAccessList[Int, RList[Int]] = new BinaryRandomAccessList[Int]
}
