package finger

import okasaki.{CatenableDequeSpec, IntElements}

class FingerTreeDequeSpec extends CatenableDequeSpec(new FingerTreeDeque[Int])
  with IntElements