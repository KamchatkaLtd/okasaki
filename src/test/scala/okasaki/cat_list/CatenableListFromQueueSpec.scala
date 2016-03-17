package okasaki.cat_list

import okasaki.misc.Susp
import okasaki.queues.HoodMelvilleQueue
import okasaki.{CatenableListSpec, IntElements}

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
class CatenableListFromQueueSpec
  extends CatenableListSpec(
    new CatenableListFromQueue[Int, HoodMelvilleQueue.Repr] {
      val q = new HoodMelvilleQueue[Susp[CL]]
    })
  with IntElements