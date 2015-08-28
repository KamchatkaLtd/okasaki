package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class RealTimeDequeSpec
  extends DequeSpec(new RealTimeDeque[Int](3))
  with IntElements
