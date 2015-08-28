package okasaki

import scala.collection.immutable.Stream.iterate

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class DequeSpec[E, Q](deque: Deque[E, Q]) extends OutputRestrictedDequeSpec(deque) {
  "A deque" should {
    "Maintain the reverse order" ! prop {
      xs: List[E] =>
        val xs1 = drainReversed(fromListReversed(xs))
        xs1 === xs
    }
  }

  def drainReversed(q: Q): List[E] =
    iterate(q)(deque.init)
      .takeWhile(!deque.isEmpty(_))
      .map(deque.last)
      .toList
}
