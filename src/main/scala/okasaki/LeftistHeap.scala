package okasaki

import okasaki.LeftistHeap._

import scala.annotation.tailrec

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object LeftistHeap {
  object Empty extends Repr[Nothing] {
    override def toString: String = "E"
  }

  case class SubHeap[E](rank: Int, x: E, left: Repr[E], right: Repr[E]) extends Repr[E] {
    override def toString: String = s"T($rank,$x,$left,$right)"
  }

  sealed trait Repr[+E]

  // Ex 3.3
  def fromList[E, EH](l: List[E])(implicit ord: Ordering[E], heap: Heap[E, EH]): EH = {
    @tailrec
    def pairs(l: List[EH], a: List[EH]): List[EH] = l match {
      case Nil => a
      case x :: Nil => x :: a
      case x :: y :: t =>
        val xy = heap.merge(x, y)
        pairs(t, xy :: a)
    }

    def fromList1(l: List[EH]): EH = l match {
      case Nil => heap.empty
      case x :: Nil => x
      case _ => fromList1(pairs(l, Nil))
    }

    fromList1(l.map(heap.insert(_, heap.empty)))
  }
}

// Fig 3.2
class LeftistHeap[E](implicit val ord: Ordering[E]) extends Heap[E, Repr[E]] {

  import okasaki.LeftistHeap._

  override val empty: Repr[E] = Empty

  override val isEmpty: (Repr[E]) => Boolean = {
    case Empty => true
    case _ => false
  }

  override val merge: (Repr[E], Repr[E]) => Repr[E] = {
    case (h, Empty) => h
    case (Empty, h) => h
    case (h1@SubHeap(_, x, a1, b1), h2@SubHeap(_, y, a2, b2)) =>
      if (ord.lteq(x, y)) makeT(x, a1, merge(b1, h2))
      else makeT(y, a2, merge(h1, b2))
  }

  // Ex. 3.2
  override val insert: (E, Repr[E]) => Repr[E] = {
    case (x, Empty) => SubHeap(1, x, Empty, Empty)
    case (x, SubHeap(_, y, a, b)) =>
      if (ord.lteq(x, y)) makeT(x, a, insert(y, b))
      else makeT(y, a, insert(x, b))
  }

  override val findMin: (Repr[E]) => E = {
    case SubHeap(_, x, _, _) => x
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  override val deleteMin: (Repr[E]) => Repr[E] = {
    case SubHeap(_, _, a, b) => merge(a, b)
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
  }

  private val rank: Repr[_] => Int = {
    case Empty => 0
    case SubHeap(r, _, _, _) => r
  }

  private def makeT(x: E, a: Repr[E], b: Repr[E]): Repr[E] =
    if (rank(a) >= rank(b)) SubHeap(rank(b) + 1, x, a, b)
    else SubHeap(rank(a) + 1, x, b, a)
}
