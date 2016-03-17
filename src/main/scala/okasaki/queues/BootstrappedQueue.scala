package okasaki.queues

import okasaki.Queue
import okasaki.misc.Susp
import okasaki.queues.BootstrappedQueue._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

object BootstrappedQueue {


  sealed trait BQueue[+E]

  object Empty extends BQueue[Nothing] {
    override def toString = s"Empty"
  }

  case class Q[E](lenfm: Int, f: List[E], m: SuspQ[E], lenr: Int, r: List[E]) extends BQueue[E]

  type SuspQ[E] = BQueue[Susp[List[E]]]

  def checkQ[E](lenfm: Int, f: List[E], m: SuspQ[E], lenr: Int, r: List[E]): BQueue[E] =
    if (lenr <= lenfm) checkF(lenfm, f, m, lenr, r)
    else {
      val newm = snoc(m, Susp(r.reverse))
      checkF(lenfm + lenr, f, newm, 0, Nil)
    }

  def checkF[E](lenfm: Int, f: List[E], m: SuspQ[E], lenr: Int, r: List[E]): BQueue[E] =
    (lenfm, f, m, lenr, r) match {
      case (_, Nil, Empty, _, _) => Empty
      case (_, Nil, m1, _, _) => Q(lenfm, head(m1)(), tail(m1), lenr, r)
      case _ => Q(lenfm, f, m, lenr, r)
    }

  def snoc[E](q: BQueue[E], e: E): BQueue[E] = q match {
    case Empty => Q(1, List(e), Empty, 0, Nil)
    case Q(lenfm, f, m, lenr, r) => checkQ(lenfm, f, m.asInstanceOf[SuspQ[E]], lenr + 1, e :: r) // GATD skolem
  }

  def head[E](q: BQueue[E]): E = q match {
    case Empty => throw new IllegalArgumentException("Called head() on an empty queue")
    case Q(_, x :: _, _, _, _) => x
  }

  def tail[E](q: BQueue[E]): BQueue[E] = q match {
    case Empty => throw new IllegalArgumentException("Called tail() on an empty queue")
    case Q(lenfm, x :: f1, m, lenr, r) => checkQ(lenfm - 1, f1, m.asInstanceOf[SuspQ[E]], lenr, r) // GATD skolem
  }
}

class BootstrappedQueue[E] extends Queue[E, BQueue[E]] {

  override def empty: BQueue[E] = Empty

  override def isEmpty(q: BQueue[E]): Boolean = q == Empty

  override def snoc(q: BQueue[E], x: E): BQueue[E] = BootstrappedQueue.snoc(q, x)

  override def tail(q: BQueue[E]): BQueue[E] = BootstrappedQueue.tail(q)

  override def head(q: BQueue[E]): E = BootstrappedQueue.head(q)
}
