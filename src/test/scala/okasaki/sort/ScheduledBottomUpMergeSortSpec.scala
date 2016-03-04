package okasaki.sort

import okasaki.{IntElements, SortingSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ScheduledBottomUpMergeSortSpec
  extends SortingSpec(new ScheduledBottomUpMergeSort[Int])
  with IntElements