package okasaki

import org.scalacheck.{Gen, Arbitrary}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BottomUpMergeSortSpec extends SortingSpec {
  override type E = Int

  override type S = BottomUpMergeSort.Repr[E]

  override def sorter: Sortable[E, S] = new BottomUpMergeSort[Int]()

  override implicit def elements: Arbitrary[E] =  Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))
}
