package okasaki.deques

import okasaki.OutputRestrictedDeque.ForQueue
import okasaki.queues.HoodMelvilleQueue
import okasaki.{IntElements, OutputRestrictedDequeSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class OutputRestrictedDequeFromQueueSpec
  extends OutputRestrictedDequeSpec(new ForQueue(new HoodMelvilleQueue[Int]))
  with IntElements
