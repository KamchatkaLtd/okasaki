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
        val q = fromList(xs)
        val xs1 = drain(q)
        xs1 === xs
    }
  }

  "An empty queue" should {
    "allow snoc" ! prop {
      e: E =>
        val q = queue.snoc(queue.empty, e)
        queue.head(q) === e
    }

    "be empty for head" ! prop {
      e: E =>
        val q = queue.empty
        queue.head(q) should throwAn[IllegalStateException]
    }

    "be empty for tail" ! prop {
      e: E =>
        val q = queue.empty
        queue.tail(q) should throwAn[IllegalStateException]
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
