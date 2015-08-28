package okasaki

import okasaki.OutputRestrictedDeque.ForQueue

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class OutputRestrictedDequeFromQueueSpec
  extends OutputRestrictedDequeSpec(new ForQueue(new HoodMelvilleQueue[Int]))
  with IntElements
