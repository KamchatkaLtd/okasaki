package okasaki.heaps

import okasaki.Heap
import okasaki.heaps.LeftistHeap._

import scala.annotation.tailrec


/**
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object LeftistHeap {

  object Empty extends Repr[Nothing] {
    override def toString: String = "E"
  }

  case class SubHeap[E](rank: Int, x: E, left: Repr[E], right: Repr[E]) extends Repr[E] {
    override def toString: String = s"T($rank,$x,$left,$right)"
  }

  sealed trait Repr[+E]

  // Ex. 3.2
  private def insert[E](x: E, h: Repr[E])(implicit ord: Ordering[E]): Repr[E] = h match {
    case Empty => SubHeap(1, x, Empty, Empty)
    case SubHeap(_, y, a, b) =>
      if (ord.lteq(x, y)) makeT(x, a, insert(y, b))
      else makeT(y, a, insert(x, b))
  }

  private def rank(t: Repr[_]): Int = t match {
    case Empty => 0
    case SubHeap(r, _, _, _) => r
  }

  private def makeT[E](x: E, a: Repr[E], b: Repr[E]): Repr[E] =
    if (rank(a) >= rank(b)) SubHeap(rank(b) + 1, x, a, b)
    else SubHeap(rank(a) + 1, x, b, a)

  private def merge[E](a: Repr[E], b: Repr[E])(implicit ord: Ordering[E]): Repr[E] =
    (a, b) match {
      case (_, Empty) => a
      case (Empty, _) => b
      case (h1@SubHeap(_, x, a1, b1), h2@SubHeap(_, y, a2, b2)) =>
        if (ord.lteq(x, y)) makeT(x, a1, merge(b1, h2))
        else makeT(y, a2, merge(h1, b2))
    }


  // Ex 3.3
  def fromList[E](l: List[E])(implicit ord: Ordering[E]): LeftistHeap[E] = {
    @tailrec
    def pairs(l: List[Repr[E]], a: List[Repr[E]]): List[Repr[E]] = l match {
      case Nil => a
      case x :: Nil => x :: a
      case x :: y :: t =>
        val xy = merge(x, y)
        pairs(t, xy :: a)
    }

    def fromList1(l: List[Repr[E]]): Repr[E] = l match {
      case Nil => Empty
      case x :: Nil => x
      case _ => fromList1(pairs(l, Nil))
    }

    new LeftistHeap[E](fromList1(l.map(insert(_, Empty.asInstanceOf[Repr[E]]))))
  }
}

// Fig 3.2
class LeftistHeap[E](val h: Repr[E] = Empty)
                    (implicit val ord: Ordering[E])
  extends Heap[E, LeftistHeap[E]] {

  override def empty = new LeftistHeap[E](Empty)

  override def isEmpty: Boolean = h == Empty

  override def merge(o: LeftistHeap[E]) = new LeftistHeap[E](LeftistHeap.merge(h, o.h))

  override def insert(x: E) = new LeftistHeap[E](LeftistHeap.insert(x, h))

  override def findMin: E = h match {
    case SubHeap(_, x, _, _) => x
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
  }

  override def deleteMin: LeftistHeap[E] = h match {
    case SubHeap(_, _, a, b) => new LeftistHeap[E](LeftistHeap.merge(a, b))
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
  }
}
