package okasaki

import scala.collection.immutable.Stream.iterate

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class DequeSpec[E, Q](queue: Deque[E, Q]) extends OutputRestrictedDequeSpec(queue) {
  "A deque" should {
    "Maintain the reverse order" ! prop {
      xs: List[E] =>
        val xs1 = drainReversed(fromListReversed(xs))
        xs1 === xs
    }
  }

  def drainReversed(q: Q): List[E] =
    iterate(q)(queue.init)
      .takeWhile(!queue.isEmpty(_))
      .map(queue.last)
      .toList
}
