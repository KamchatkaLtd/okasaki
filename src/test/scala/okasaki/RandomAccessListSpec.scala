package okasaki

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.choose
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class RandomAccessListSpec[E, RL, RAL <: RandomAccessList[E, RL]](val list: RAL)
  extends Specification with ScalaCheck {

  implicit def elements: Arbitrary[E]

  case class ListWithIndex(l: List[E], i: Int) {
    assert(i >= 0 && i < l.size)
  }

  case class ListWithTwoIndices(l: List[E], i: Int, j: Int) {
    assert(i >= 0 && i < l.size)
    assert(j >= 0 && j < l.size)

    def distinct: Boolean = i != j
  }

  "A list" should {
    "Maintain the order" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromList(xs))
        xs1 === xs
    }

    "Allow random lookup" ! prop {
      xs: ListWithIndex =>
        val ListWithIndex(l, i) = xs

        val xs1 = fromList(l)

        list.lookup(i, xs1) === l(i)
    }

    "Allow random update" ! prop {
      (xs: ListWithIndex, e: E) =>
        val ListWithIndex(l, i) = xs

        val xs1 = list.update(i, e, fromList(l))

        list.lookup(i, xs1) === e
    }

    "Random update is non-breaking" ! prop {
      (xs: ListWithTwoIndices, e: E) => xs.distinct ==>  {
        val ListWithTwoIndices(l, i, j) = xs

        val xs1 = list.update(i, e, fromList(l))

        list.lookup(j, xs1) === l(j)
      }
    }
  }

  def fromList(xs: List[E]): RL =
    xs.reverse.foldLeft(list.empty)(list.cons)

  def drain(xs: RL): List[E] =
    if (list.isEmpty(xs)) Nil
    else list.head(xs) :: drain(list.tail(xs))

  lazy implicit val listWithIndex: Arbitrary[ListWithIndex] =
    Arbitrary(for {
      xs <- arbitrary[List[E]]
      i <- choose(0, xs.size - 1)
    } yield ListWithIndex(xs, i))

  lazy implicit val listWithTwoDistinctIndexes: Arbitrary[ListWithTwoIndices] =
    Arbitrary(for {
      xs <- arbitrary[List[E]]
      i <- choose(0, xs.size - 1)
      j <- choose(0, xs.size - 1)
    } yield ListWithTwoIndices(xs, i, j))
}
