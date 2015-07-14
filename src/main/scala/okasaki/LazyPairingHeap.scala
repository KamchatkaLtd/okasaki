package okasaki


object LazyPairingHeap {

  sealed trait Repr[+E]

  object Empty extends Repr[Nothing] {
    override def toString = s"E"
  }

  case class Tree[E](e: E, odd: Repr[E], lts: Susp[Repr[E]]) extends Repr[E] {
    override def toString: String = s"T($e,$odd,$lts)"
  }

}

class LazyPairingHeap[E](implicit val ord: Ordering[E]) extends Heap[E, LazyPairingHeap.Repr[E]] {

  import okasaki.LazyPairingHeap._

  override def empty: Repr[E] = Empty

  override def isEmpty: (Repr[E]) => Boolean = _ == Empty

  def link(t1: Tree[E], t2: Tree[E]): Repr[E] = (t1, t2) match {
    case (Tree(x, Empty, m), a) => Tree(x, a, m)
    case (Tree(x, b, m), a) => Tree(x, Empty, Susp(merge(merge(a, b), m())))
  }

  override def merge: (Repr[E], Repr[E]) => Repr[E] = {
    case (Empty, h) => h
    case (h, Empty) => h
    case (a@Tree(x, _, _), b@Tree(y, _, _)) =>
      if (ord.lteq(x, y)) link(a, b) else link(b, a)
  }

  override def insert: (E, Repr[E]) => Repr[E] = {
    case (x, t) =>
      merge(Tree(x, Empty, Susp[Repr[E]](Empty)), t)
  }

  def mergePairs(hs: List[Repr[E]]): Repr[E] = hs match {
    case Nil => Empty
    case h :: Nil => h
    case h1 :: h2 :: rest => merge(merge(h1, h2), mergePairs(rest))
  }

  override def findMin: (Repr[E]) => E = {
    case Empty => throw new IllegalStateException("called findMin on an empty heap")
    case Tree(x, _, _) => x
  }

  override def deleteMin: (Repr[E]) => Repr[E] = {
    case Empty => throw new IllegalStateException("called deleteMin on an empty heap")
    case Tree(_, a, m) => merge(a, m())
  }
}
