package okasaki

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Try

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
abstract class FiniteMapSpec[K, V, M] extends Specification with ScalaCheck {

  implicit def keys: Arbitrary[K]

  implicit def elements: Arbitrary[V]

  def map: FiniteMap[K, V, M]

  "An empty map" should {
    "contain no keys" ! prop { k: K =>
      map.lookup(k, map.empty) must throwA[FiniteMap.NotFound[K]]
    }
  }

  "A non-empty map" should {
    "contain its elements" ! prop { a: Map[K, V] =>
      val s = from(a)

      shouldHaveTheSameElements(a, s)
    }

    "not contain extra elements" ! prop { (a: Map[K, V], e: K) =>
      val s = from(a)

      Try(map.lookup(e, s)).isSuccess === a.get(e).isDefined
    }
  }

  "A map" should {
    "be amendable" ! prop { (a: Map[K, V], v: V) =>
      val s = from(a)

      forall(a.keys) { k =>
        shouldHaveTheSameElements(a + (k -> v), map.bind(k, v, s))
      }
    }
  }

  def shouldHaveTheSameElements(a: Map[K, V], s: M) = {
    forall(a) {
      case (k, v) => map.lookup(k, s) === v
    }
  }

  private def from(data: Map[K, V]): M =
    data.foldLeft(map.empty) {
      case (a, (k, v)) => map.bind(k, v, a)
    }
}
