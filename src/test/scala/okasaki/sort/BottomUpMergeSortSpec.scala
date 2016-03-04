package okasaki.sort

import okasaki.{IntElements, SortingSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BottomUpMergeSortSpec
  extends SortingSpec(new BottomUpMergeSort[Int])
  with IntElements