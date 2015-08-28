package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class HoodMelvilleQueueSpec extends QueueSpec[Int, HoodMelvilleQueue.Repr[Int]] with IntElements {
  override def queue: Queue[Int, HoodMelvilleQueue.Repr[Int]] = new HoodMelvilleQueue[Int]
}
