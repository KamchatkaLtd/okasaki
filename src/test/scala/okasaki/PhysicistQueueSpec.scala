package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class PhysicistQueueSpec extends QueueSpec[Int, PhysicistQueue.Repr[Int]] with IntElements {
  override def queue: Queue[Int, PhysicistQueue.Repr[Int]] = new PhysicistQueue[Int] {}
}
