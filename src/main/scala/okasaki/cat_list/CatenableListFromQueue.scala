package okasaki.cat_list

import okasaki.cat_list.CatenableListFromQueue._
import okasaki.misc.Susp
import okasaki.{CatenableList, Queue}

/**
* Copyright (C) 2016 Kamchatka Ltd
*/
object CatenableListFromQueue {

  sealed trait CatList[+Q[_], +E]

  object Empty extends CatList[Nothing, Nothing]

  case class C[Q[_], E](x: E, q: Q[Susp[CatList[Q, E]]]) extends CatList[Q, E]
}

class CatenableListFromQueue[E, QBS[_]](q: Queue[Susp[CatList[QBS, E]], QBS[Susp[CatList[QBS, E]]]])
      extends CatenableList[E, CatList[QBS, E]] {

  type CL = CatList[QBS, E]

  def just(e: E): CL = C(e, q.empty)

  def link(a: CL, b: Susp[CL]): CL = {
    val C(x, a1) = a
    C(x, q.snoc(a1, b))
  }

  def linkAll(cls: QBS[Susp[CL]]): CL = {
    val t = q.head(cls)
    val q1 = q.tail(cls)
    if (q.isEmpty(q1)) t.apply() else link(t(), Susp(linkAll(q1)))
  }

  val empty = Empty

  def isEmpty = _ == Empty

  def cons = {
    case (e, cl) => link(just(e), Susp(cl))
  }

  def snoc = {
    case (cl, e) => link(cl, Susp(just(e)))
  }

  def ++ = {
    case (Empty, y) => y
    case (x, Empty) => x
    case (x, y) => link(x, Susp(y))
  }

  def head = {
    case Empty => throw new IllegalArgumentException("head called on an empty list")
    case C(e, _) => e
  }

  def tail = {
    case Empty => throw new IllegalArgumentException("head called on an empty list")
    case C(_, cls) if q.isEmpty(cls) => Empty
    case C(_, cls) => linkAll(cls)
  }
}
