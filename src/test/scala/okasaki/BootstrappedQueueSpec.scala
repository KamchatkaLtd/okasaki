package okasaki

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
class BootstrappedQueueSpec
  extends QueueSpec(new BootstrappedQueue[Int])
  with IntElements
