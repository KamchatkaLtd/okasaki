package okasaki

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.choose
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class RandomAccessListSpec[E, RL](list: RandomAccessList[E, RL]) extends Specification with ScalaCheck {
  implicit def elements: Arbitrary[E]

  "A list" should {
    "Maintain the order" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromList(xs))
        xs1 === xs
    }

    "Allow random lookup" ! prop {
      xs: (List[E], Int) =>
        val (l, i) = xs

        val xs1 = fromList(l)

        list.lookup(i, xs1) === l(i)
    }

    "Allow random update" ! prop {
      (xs: (List[E], Int), e: E) =>
        val (l, i) = xs

        val xs1 = list.update(i, e, fromList(l))

        list.lookup(i, xs1) === e
    }

    "Random update is non-breaking" ! prop {
      (xs: (List[E], Int, Int), e: E) => (xs._2 != xs._3) ==> {
        val (l, i, j) = xs

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

  lazy implicit val listWithIndex: Arbitrary[(List[E], Int)] =
    Arbitrary(for {
      xs <- arbitrary[List[E]]
      i <- choose(0, xs.size - 1)
    } yield xs -> i)

  lazy implicit val listWithTwoDistinctIndexes: Arbitrary[(List[E], Int, Int)] =
    Arbitrary(for {
      xs <- arbitrary[List[E]]
      i <- choose(0, xs.size - 1)
      j <- choose(0, xs.size - 1)
    } yield (xs, i, j))
}
