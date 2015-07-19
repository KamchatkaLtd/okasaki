package okasaki


import okasaki.WeightBiasedLeftistHeap.Repr

object WeightBiasedLeftistHeap {

  object Empty extends Repr[Nothing] {
    override def toString: String = "E"
  }

  case class SubHeap[E](weight: Long, x: E, left: Repr[E], right: Repr[E]) extends Repr[E] {
    override def toString: String = s"T($weight,$x,$left,$right)"
  }

  sealed trait Repr[+E]
}

class WeightBiasedLeftistHeap[E](implicit val ord: Ordering[E]) extends Heap[E, Repr[E]] {

  import okasaki.WeightBiasedLeftistHeap._

  override val empty: Repr[E] = Empty

  override val isEmpty: (Repr[E]) => Boolean = {
    case Empty => true
    case _ => false
  }

  override val merge: (Repr[E], Repr[E]) => Repr[E] = {
    case (h, Empty) => h
    case (Empty, h) => h
    case (h1@SubHeap(w1, x, a1, b1), h2@SubHeap(w2, y, a2, b2)) =>
      val w = w1 + w2
      if (ord.lteq(x, y)) SubHeap(w, x, a1, merge(b1, h2))
      else SubHeap(w, y, a2, merge(h1, b2))
  }

  override val insert: (E, Repr[E]) => Repr[E] = {
    case (x, Empty) => SubHeap(1, x, Empty, Empty)
    case (x, SubHeap(w, y, a, b)) =>
      if (ord.lteq(x, y)) SubHeap(w + 1, x, a, insert(y, b))
      else SubHeap(w + 1, y, a, insert(x, b))
  }

  override val findMin: (Repr[E]) => E = {
    case SubHeap(_, x, _, _) => x
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  override val deleteMin: (Repr[E]) => Repr[E] = {
    case SubHeap(_, _, a, b) => merge(a, b)
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
  }
}
