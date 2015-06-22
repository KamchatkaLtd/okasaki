package okasaki


class WeightBiasedLeftistHeapOps[E](implicit val ord: Ordering[E]) extends Heap[E, WeightBiasedLeftistHeap[E]] {

  import okasaki.WeightBiasedLeftistHeap._

  override val empty: WeightBiasedLeftistHeap[E] = Empty

  override val isEmpty: (WeightBiasedLeftistHeap[E]) => Boolean = {
    case Empty => true
    case _ => false
  }

  override val merge: (WeightBiasedLeftistHeap[E], WeightBiasedLeftistHeap[E]) => WeightBiasedLeftistHeap[E] = {
    case (h, Empty) => h
    case (Empty, h) => h
    case (h1@SubHeap(w1, x, a1, b1), h2@SubHeap(w2, y, a2, b2)) =>
      val w = w1 + w2
      if (ord.lteq(x, y)) SubHeap(w, x, a1, merge(b1, h2))
      else SubHeap(w, y, a2, merge(h1, b2))
  }

  override val insert: (E, WeightBiasedLeftistHeap[E]) => WeightBiasedLeftistHeap[E] = {
    case (x, Empty) => SubHeap(1, x, Empty, Empty)
    case (x, SubHeap(w, y, a, b)) =>
      if (ord.lteq(x, y)) SubHeap(w + 1, x, a, insert(y, b))
      else SubHeap(w + 1, y, a, insert(x, b))
  }

  override val findMin: (WeightBiasedLeftistHeap[E]) => E = {
    case SubHeap(_, x, _, _) => x
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  override val deleteMin: (WeightBiasedLeftistHeap[E]) => WeightBiasedLeftistHeap[E] = {
    case SubHeap(_, _, a, b) => merge(a, b)
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
  }
}
