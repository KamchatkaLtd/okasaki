package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BottomUpMergeSortSpec extends SortingSpec with IntElements {
  override type E = Int

  override type S = BottomUpMergeSort.Repr[E]

  override def sorter: Sortable[E, S] = new BottomUpMergeSort[Int]()
}
