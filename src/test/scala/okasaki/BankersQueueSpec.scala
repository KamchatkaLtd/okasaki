package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BankersQueueSpec extends QueueSpec[Int, (Int, Stream[Int], Int, Stream[Int])] with IntElements {
  override def queue: Queue[Int, (Int, Stream[Int], Int, Stream[Int])] = new BankersQueue[Int] {}
}
