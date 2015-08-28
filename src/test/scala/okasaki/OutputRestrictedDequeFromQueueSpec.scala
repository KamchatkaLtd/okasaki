package okasaki

import okasaki.OutputRestrictedDeque.{ForQueue, Repr}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class OutputRestrictedDequeFromQueueSpec
  extends OutputRestrictedDequeSpec[Int, Repr[Int, HoodMelvilleQueue.Repr[Int]]] with IntElements {
  override def queue = new ForQueue(new HoodMelvilleQueue[Int])
}
