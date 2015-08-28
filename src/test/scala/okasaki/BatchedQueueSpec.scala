package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BatchedQueueSpec extends QueueSpec[Int, (List[Int], List[Int])] with IntElements {
  override def queue: Queue[Int, (List[Int], List[Int])] = new BatchedQueue[Int] {}
}
