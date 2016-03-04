package okasaki.sort

import okasaki.Sortable
import okasaki.misc.Susp
import okasaki.sort.BottomUpMergeSort._

import scala.annotation.tailrec

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object BottomUpMergeSort {
  type Repr[E] = (Int, Susp[List[List[E]]])
}

class BottomUpMergeSort[E](implicit val ord: Ordering[E]) extends Sortable[E, Repr[E]] {
  override def empty: Repr[E] = (0, Susp(Nil))

  def mrg(a: List[E], b: List[E]): List[E] = (a, b) match {
    case (xs, Nil) => xs
    case (Nil, ys) => ys
    case (x :: xs, y :: ys) => if (ord.lt(x, y)) x :: mrg(xs, b) else y :: mrg(a, ys)
  }

  override def add(e: E, s: Repr[E]): Repr[E] = {
    @tailrec
    def addSeg(seg: List[E], segs: List[List[E]], size: Int): List[List[E]] =
      if (size % 2 == 0) seg :: segs
      else addSeg(mrg(seg, segs.head), segs.tail, size / 2)

    val (size, segs) = s
    (size + 1, Susp(addSeg(List(e), segs(), size)))
  }


  override def sort(s: Repr[E]): List[E] =
    s._2().foldLeft(List[E]())(mrg)
}
