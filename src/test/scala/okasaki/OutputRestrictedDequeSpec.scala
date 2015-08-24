package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait OutputRestrictedDequeSpec[E, Q] extends QueueSpec[E, Q] {

  def queue: OutputRestrictedDeque[E, Q]

  "An output-restricted deque" should {
    "Maintain the reverse order" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromListReversed(xs))
        xs1 === xs.reverse
    }
  }

  def fromListReversed(xs: List[E]): Q = xs.foldLeft(queue.empty)(queue.cons)

}
