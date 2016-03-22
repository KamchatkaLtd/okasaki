package okasaki.heaps

import okasaki.Heap
import okasaki.heaps.PairingHeap.{Empty, PHeap}

object PairingHeap {

  sealed trait PHeap[+E]

  object Empty extends PHeap[Nothing] {
    override def toString = s"E"
  }

  case class Tree[E](e: E, ts: List[Tree[E]]) extends PHeap[E] {
    override def toString: String = s"T($e,$ts)"
  }

}

class PairingHeap[E](val h: PHeap[E] = Empty)
                    (implicit val ord: Ordering[E])
  extends Heap[E, PairingHeap[E]] {

  import okasaki.heaps.PairingHeap._

  override def empty = new PairingHeap[E](Empty)

  override def isEmpty: Boolean = h == Empty

  override def merge(o: PairingHeap[E]): PairingHeap[E] = new PairingHeap[E](merge(h, o.h))

  private def merge(a: PHeap[E], b: PHeap[E]): PHeap[E] = (a, b) match {
    case (Empty, _) => b
    case (_, Empty) => a
    case (h1@Tree(x, hs1), h2@Tree(y, hs2)) =>
      if (ord.lteq(x, y)) Tree(x, h2 :: hs1)
      else Tree(y, h1 :: hs2)
  }

  override def insert(x: E) = new PairingHeap[E](merge(Tree(x, Nil), h))

  def mergePairs(hs: List[PHeap[E]]): PHeap[E] = hs match {
    case Nil => Empty
    case hh :: Nil => hh
    case h1 :: h2 :: rest => merge(merge(h1, h2), mergePairs(rest))
  }

  override def findMin: E = h match {
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
    case Tree(x, _) => x
  }

  override def deleteMin = h match {
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
    case Tree(_, hs) => new PairingHeap[E](mergePairs(hs))
  }
}
