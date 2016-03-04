package okasaki.deques

import okasaki.{DequeSpec, IntElements}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BatchedDequeSpec
  extends DequeSpec(new BatchedDeque[Int])
  with IntElements
