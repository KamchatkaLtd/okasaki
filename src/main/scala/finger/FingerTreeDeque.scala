package finger

import finger.Fingers.{Empty, FingerTree}
import okasaki.CatenableDeque

class FingerTreeDeque[E] extends CatenableDeque[E, FingerTree[E]] {
  override def init(q: FingerTree[E]): FingerTree[E] = Fingers.init(q)

  override def last(q: FingerTree[E]): E = Fingers.last(q)

  override def cons(e: E, q: FingerTree[E]): FingerTree[E] = Fingers.cons(e, q)

  override def empty: FingerTree[E] = Empty

  override def isEmpty(q: FingerTree[E]): Boolean = Fingers.isEmpty(q)

  override def snoc(q: FingerTree[E], e: E): FingerTree[E] = Fingers.snoc(q, e)

  override def head(q: FingerTree[E]): E = Fingers.head(q)

  override def tail(q: FingerTree[E]): FingerTree[E] = Fingers.tail(q)

  override def ++(a: FingerTree[E], b: FingerTree[E]): FingerTree[E] = Fingers.concat(a, b)
}
