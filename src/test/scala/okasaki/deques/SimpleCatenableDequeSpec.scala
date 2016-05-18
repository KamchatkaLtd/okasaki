package okasaki.deques

import okasaki.deques.RealTimeDeque.Repr
import okasaki.deques.SimpleCatenableDeque.DequeLike
import okasaki.deques.SimpleCatenableDequeSpec.FromRealTimeDeque
import okasaki.{CatenableDequeSpec, Deque, IntElements}

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object SimpleCatenableDequeSpec {
  class FromRealTimeDeque[T] extends SimpleCatenableDeque[T, RealTimeDeque.Repr] {
    override implicit def ud: DequeLike[Repr] = new DequeLike[Repr] {
      override def deque[E]: Deque[E, Repr[E]] = new RealTimeDeque(3)
    }
  }
}

class SimpleCatenableDequeSpec
  extends CatenableDequeSpec[Int, SimpleCatenableDeque.Cat[Int, RealTimeDeque.Repr]](new FromRealTimeDeque[Int]) with IntElements


