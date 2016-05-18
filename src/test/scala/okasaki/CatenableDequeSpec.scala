package okasaki

/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
abstract class CatenableDequeSpec[E, Q](deque: CatenableDeque[E, Q]) extends DequeSpec(deque) {
  "A catenable deque" should {
    "Maintain the order (R)" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromListR(xs))
        xs1 === xs
    }
    "Maintain the order (L)" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromListL(xs))
        xs1 === xs
    }
    "Support concatenation (LL)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListL(a), fromListL(b)))
        res === (a ++ b)
    }
    "Support concatenation (LR)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListL(a), fromListR(b)))
        res === (a ++ b)
    }
    "Support concatenation (RL)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListR(a), fromListL(b)))
        res === (a ++ b)
    }
    "Support concatenation (RR)" ! prop {
      (a: List[E], b: List[E]) =>
        val res = drain(deque.++(fromListR(a), fromListR(b)))
        res === (a ++ b)
    }
  }

  def fromListR(xs: List[E]): Q =
    xs.foldRight(deque.empty)(deque.cons)

  def fromListL(xs: List[E]): Q =
    xs.foldLeft(deque.empty)(deque.snoc)
}
