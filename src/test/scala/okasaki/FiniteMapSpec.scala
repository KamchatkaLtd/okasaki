package okasaki

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Try

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
abstract class FiniteMapSpec[K, V] extends Specification with ScalaCheck {

  implicit def keys: Arbitrary[K]

  implicit def elements: Arbitrary[V]

  def map: FiniteMap[K, V]

  "An empty map" should {
    "contain no keys" ! prop { k: K =>
      map.empty.lookup(k) must throwA[FiniteMap.NotFound[K]]
    }
  }

  "A non-empty map" should {
    "contain its elements" ! prop { a: Map[K, V] =>
      val s = from(a)

      a forall {
        case (k, v) => s.lookup(k) === v
      }
    }

    "not contain extra elements" ! prop { (a: Map[K, V], e: K) =>
      val s = from(a)

      Try(s.lookup(e)).isSuccess === a.get(e).isDefined
    }
  }

  private def from(data: Map[K, V]): FiniteMap[K, V] =
    data.foldLeft(map.empty) {
      case (a, (k, v)) => a.bind(k, v)
    }
}
