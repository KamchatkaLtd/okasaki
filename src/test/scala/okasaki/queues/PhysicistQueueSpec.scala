package okasaki.queues

import okasaki.{IntElements, QueueSpec}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class PhysicistQueueSpec
  extends QueueSpec(new PhysicistQueue[Int])
  with IntElements
