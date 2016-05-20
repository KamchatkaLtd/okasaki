package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class OutputRestrictedDequeSpec[E, Q](ordeque: OutputRestrictedDeque[E, Q]) extends QueueSpec(ordeque) {
  "An output-restricted deque" should {
    "Maintain the reverse order" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromListReversed(xs))
        xs1 === xs.reverse
    }
  }

  "An empty ordeque" should {
    "allow cons" ! prop {
      e: E =>
        val q = ordeque.cons(e, ordeque.empty)
        ordeque.head(q) === e
    }
  }


  def fromListReversed(xs: List[E]): Q = xs.foldLeft(ordeque.empty)((q: Q, e: E) => ordeque.cons(e, q))

}
