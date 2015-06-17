package okasaki

import scala.collection.immutable.{List => ConsList}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object LeftistHeapOps {
  // Ex 3.3
  def fromList[E, EH](l: ConsList[E])(implicit ord: Ordered[E], list: List[E], heap: Heap[E, EH]): EH = {
    if (list.isEmpty(l)) heap.empty
    else heap.insert(list.head(l), fromList(list.tail(l)))
  }
}

class LeftistHeapOps[E](implicit val ord: Ordered[E]) extends Heap[E, LeftistHeap[E]] {

  import okasaki.LeftistHeap._

  override val empty: LeftistHeap[E] = Empty

  override val isEmpty: (LeftistHeap[E]) => Boolean = {
    case LeftistHeap.Empty => true
    case _ => false
  }

  override val merge: (LeftistHeap[E], LeftistHeap[E]) => LeftistHeap[E] = {
    case (h, Empty) => h
    case (Empty, h) => h
    case (h1@SubHeap(_, x, a1, b1), h2@SubHeap(_, y, a2, b2)) =>
      if (ord.leq(x, y)) makeT(x, a1, merge(b1, h2))
      else makeT(y, a2, merge(h1, b2))
  }

  // Ex. 3.2
  override val insert: (E, LeftistHeap[E]) => LeftistHeap[E] = {
    case (x, Empty) => SubHeap(1, x, Empty, Empty)
    case (x, SubHeap(_, y, a, b)) =>
      if (ord.leq(x, y)) makeT(x, a, insert(y, b))
      else makeT(y, a, insert(x, b))
  }

  override val findMin: (LeftistHeap[E]) => E = {
    case SubHeap(_, x, _, _) => x
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  override val deleteMin: (LeftistHeap[E]) => LeftistHeap[E] = {
    case SubHeap(_, _, a, b) => merge(a, b)
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  private val rank: LeftistHeap[_] => Int = {
    case Empty => 0
    case SubHeap(r, _, _, _) => r
  }

  private def makeT(x: E, a: LeftistHeap[E], b: LeftistHeap[E]): LeftistHeap[E] =
    if (rank(a) >= rank(b)) SubHeap(rank(b) + 1, x, a, b)
    else SubHeap(rank(a) + 1, x, b, a)
}
