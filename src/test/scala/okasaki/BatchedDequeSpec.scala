package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BatchedDequeSpec extends DequeSpec[Int, (List[Int], List[Int])] with IntElements {
  override def queue: Deque[Int, (List[Int], List[Int])] = new BatchedDeque[Int] {}
}
