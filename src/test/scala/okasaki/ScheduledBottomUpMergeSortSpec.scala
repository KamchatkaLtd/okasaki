package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ScheduledBottomUpMergeSortSpec extends SortingSpec with IntElements {
  override type E = Int

  override type S = ScheduledBottomUpMergeSort.Repr[E]

  override def sorter: Sortable[E, S] = new ScheduledBottomUpMergeSort[Int]()
}
