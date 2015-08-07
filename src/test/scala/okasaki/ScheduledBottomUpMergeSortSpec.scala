package okasaki

import org.scalacheck.{Arbitrary, Gen}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ScheduledBottomUpMergeSortSpec extends SortingSpec {
  override type E = Int

  override type S = ScheduledBottomUpMergeSort.Repr[E]

  override def sorter: Sortable[E, S] = new ScheduledBottomUpMergeSort[Int]()

  override implicit def elements: Arbitrary[E] =  Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))
}
