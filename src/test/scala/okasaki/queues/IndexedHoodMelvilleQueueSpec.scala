package okasaki.queues

import okasaki.{IntElements, QueueSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class IndexedHoodMelvilleQueueSpec
  extends QueueSpec(new IndexedHoodMelvilleQueue[Int])
  with IntElements
