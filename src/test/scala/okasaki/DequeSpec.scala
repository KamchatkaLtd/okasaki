package okasaki

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.collection.immutable.Stream.iterate

/**
  * Copyright (C) 2015 Kamchatka Ltd
  */
trait DequeSpec[E, Q] extends OutputRestrictedDequeSpec[E, Q] {
   def queue: Deque[E, Q]

   "A deque" should {
     "Maintain the reverse order" ! prop {
       xs: List[E] =>
         val xs1 = drainReversed(fromListReversed(xs))
         xs1 === xs
     }
   }

   def drainReversed(q: Q): List[E] =
     iterate(q)(queue.init)
       .takeWhile(!queue.isEmpty(_))
       .map(queue.last)
       .toList
 }
