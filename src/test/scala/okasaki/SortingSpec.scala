package okasaki

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait SortingSpec extends Specification with ScalaCheck {
  type E

  type S

  def sorter: Sortable[E, S]

  implicit def elements: Arbitrary[E]

  "sorting" should {
    "be natural" ! prop {
      a: List[E] =>
        val hh = a.foldRight(sorter.empty)(sorter.add)
        sorter.sort(hh) === a.sorted(sorter.ord)
    }
  }
}
