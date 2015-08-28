package okasaki

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.collection.immutable.Stream.iterate

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
abstract class QueueSpec[E, Q](queue: Queue[E, Q]) extends Specification with ScalaCheck {
  implicit def elements: Arbitrary[E]

  "A queue" should {
    "Maintain the order" ! prop {
      xs: List[E] =>
        val xs1 = drain(fromList(xs))
        xs1 === xs
    }
  }

  def fromList(xs: List[E]): Q =
    xs.foldLeft(queue.empty)(queue.snoc)

  def drain(q: Q): List[E] =
    iterate(q)(queue.tail)
      .takeWhile(!queue.isEmpty(_))
      .map(queue.head)
      .toList
}
