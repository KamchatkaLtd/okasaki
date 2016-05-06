package okasaki.queues

import okasaki.{IntElements, QueueSpec}

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
class ImplicitQueueSpec
  extends QueueSpec(new ImplicitQueue[Int])
  with IntElements
