package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RealTimeQueueSpec extends QueueSpec[Int, RealTimeQueue.Repr[Int]] with IntElements {
  override def queue: Queue[Int, RealTimeQueue.Repr[Int]] = new RealTimeQueue[Int] {}
}
