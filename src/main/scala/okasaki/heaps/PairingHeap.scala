package okasaki.heaps

import okasaki.Heap

object PairingHeap {

  sealed trait PHeap[+E]

  object Empty extends PHeap[Nothing] {
    override def toString = s"E"
  }

  case class Tree[E](e: E, ts: List[Tree[E]]) extends PHeap[E] {
    override def toString: String = s"T($e,$ts)"
  }

}

class PairingHeap[E](implicit val ord: Ordering[E]) extends Heap[E, PairingHeap.PHeap[E]] {

  import okasaki.heaps.PairingHeap._

  override def empty: PHeap[E] = Empty

  override def isEmpty(h: PHeap[E]): Boolean = h == Empty

  override def merge(a: PHeap[E], b: PHeap[E]): PHeap[E] = (a, b) match {
    case (Empty, h) => h
    case (h, Empty) => h
    case (h1@Tree(x, hs1), h2@Tree(y, hs2)) =>
      if (ord.lteq(x, y)) Tree(x, h2 :: hs1)
      else Tree(y, h1 :: hs2)
  }

  override def insert(x: E, t: PHeap[E]): PHeap[E] = merge(Tree(x, Nil), t)

  def mergePairs(hs: List[PHeap[E]]): PHeap[E] = hs match {
    case Nil => Empty
    case h :: Nil => h
    case h1 :: h2 :: rest => merge(merge(h1, h2), mergePairs(rest))
  }

  override def findMin(h: PHeap[E]): E = h match {
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
    case Tree(x, _) => x
  }

  override def deleteMin(h: PHeap[E]): PHeap[E] = h match {
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
    case Tree(_, hs) => mergePairs(hs)
  }
}
