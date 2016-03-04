package okasaki.queues

import okasaki.{IntElements, QueueSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class HoodMelvilleQueueSpec
  extends QueueSpec(new HoodMelvilleQueue[Int])
  with IntElements
