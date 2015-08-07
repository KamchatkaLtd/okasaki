package okasaki

import okasaki.ScheduledBottomUpMergeSort._

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty


/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object ScheduledBottomUpMergeSort {
  type Schedule[E] = List[Stream[E]]
  type Segment[E] = (Stream[E], Schedule[E])
  type Repr[E] = (Int, List[Segment[E]])
}

class ScheduledBottomUpMergeSort[E](implicit val ord: Ordering[E]) extends Sortable[E, Repr[E]] {
  def mrg(a: Stream[E], b: Stream[E]): Stream[E] = (a, b) match {
    case (xs, Empty) => xs
    case (Empty, ys) => ys
    case (x #:: xs, y #:: ys) => if (ord.lt(x, y)) x #:: mrg(xs, b) else y #:: mrg(a, ys)
  }

  @tailrec
  private def exec1(s: Schedule[E]): Schedule[E] = s match {
    case Nil => Nil
    case Empty :: schedule => exec1(schedule)
    case (x #:: xs) :: schedule => xs :: schedule
  }

  def exec2(s: Segment[E]): Segment[E] = (s._1, exec1(exec1(s._2)))

  override def empty: Repr[E] = (0, Nil)

  override def add(e: E, s: Repr[E]): Repr[E] = {
    @tailrec
    def addSeg(xs: Stream[E], segs: List[Segment[E]], size: Int, rsched: Schedule[E]): List[Segment[E]] =
      if (size % 2 == 0) (xs, rsched.reverse) :: segs
      else {
        val ((xs1, Nil) :: segs1) = segs
        val xs2 = mrg(xs, xs1)
        addSeg(xs2, segs1, size / 2, xs2 :: rsched)
      }

    val (size, segs) = s
    val segs1 = addSeg(Stream(e), segs, size, Nil)
    (size + 1, segs1 map exec2)
  }


  override def sort(s: Repr[E]): List[E] = {
    @tailrec
    def mrgAll(xs: Stream[E], segs: List[Segment[E]]): Stream[E] = segs match {
      case Nil => xs
      case (xs1, _) :: segs1 => mrgAll(mrg(xs, xs1), segs1)
    }

    val (size, segs) = s
    mrgAll(Empty, segs).toList
  }
}
