package okasaki.deques

import okasaki.deques.ImplicitCatenableDeque.DequeLike
import okasaki.deques.ImplicitCatenableDequeSpec.FromVector
import okasaki.{Deque, CatenableDequeSpec, IntElements}


object ImplicitCatenableDequeSpec {
  class VectorDeque[T] extends Deque[T, Vector[T]] {
    override def init(q: Vector[T]): Vector[T] =
      if (q.isEmpty) throw new IllegalStateException("empty.init")
      else q.init

    override def last(q: Vector[T]): T =
      if (q.isEmpty) throw new IllegalStateException("empty.last")
      else q.last

    override def cons(e: T, q: Vector[T]): Vector[T] = e +: q

    override def empty: Vector[T] = Vector.empty[T]

    override def snoc(q: Vector[T], e: T): Vector[T] = q :+ e

    override def tail(q: Vector[T]): Vector[T] =
      if (q.isEmpty) throw new IllegalStateException("empty.tail")
      else q.tail

    override def isEmpty(q: Vector[T]): Boolean = q.isEmpty

    override def head(q: Vector[T]): T =
      if (q.isEmpty) throw new IllegalStateException("empty.head")
      else q.head
  }

  class FromVector[T] extends ImplicitCatenableDeque[T, Vector] {
    override implicit def ud: DequeLike[Vector] = new DequeLike[Vector] {
      override def deque[E]: Deque[E, Vector[E]] = new VectorDeque[E]()
    }
  }
}

class ImplicitCatenableDequeSpec
  extends CatenableDequeSpec[Int, ImplicitCatenableDeque.Cat[Int, Vector]](new FromVector[Int]) with IntElements


