package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object ZerolessBinary {

  sealed trait Digit

  case object One extends Digit

  case object Two extends Digit

  def inc(n: List[Digit]): List[Digit] = n match {
    case Nil => List(One)
    case One :: ds => Two :: ds
    case Two :: ds => One :: inc(ds)
  }

  def add(a: Digit, b: Digit, c: Option[Digit]): (Digit, Option[Digit]) = (a, b, c) match {
    case (One, One, None) => (Two, None)
    case (One, Two, None) => (One, Some(One))
    case (Two, One, None) => (One, Some(One))
    case (Two, Two, None) => (Two, Some(One))
    case (One, One, Some(One)) => (One, Some(One))
    case (One, Two, Some(One)) => (Two, Some(One))
    case (Two, One, Some(One)) => (Two, Some(One))
    case (Two, Two, Some(One)) => (One, Some(Two))
    case (One, One, Some(Two)) => (Two, Some(One))
    case (One, Two, Some(Two)) => (One, Some(Two))
    case (Two, One, Some(Two)) => (One, Some(Two))
    case (Two, Two, Some(Two)) => (Two, Some(Two))
  }

  def add(a: List[Digit], b: List[Digit]): List[Digit] = {
    def add1(aa: List[Digit], bb: List[Digit], c: Option[Digit]): List[Digit] = {
      (aa, bb, c) match {
        case (Nil, _, None) => bb
        case (Nil, _, Some(cc)) => add1(List(cc), bb, None)
        case (_, Nil, _) => add1(bb, aa, c)
        case (x :: a1, y :: b1, z) =>
          val (sum, carry) = add(x, y, z)
          sum :: add1(a1, b1, carry)
      }
    }

    add1(a, b, None)
  }

  def toZeroless(n: Int): List[Digit] = n match {
    case 0 => Nil
    case 1 => List(One)
    case 2 => List(Two)
    case _ =>
      val half = toZeroless(n / 2)
      val twoHalves = add(half, half)

      if (n % 2 == 0) twoHalves
      else inc(twoHalves)
  }

  def value(d: Digit) = d match {
    case One => 1
    case Two => 2
  }

  def toLong(n: List[Digit]): Long = n.foldRight(0L)(value(_) + _ * 2)
}
