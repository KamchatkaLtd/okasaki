package finger

import okasaki.{DequeSpec, IntElements}

class FingerTreeDequeSpec extends DequeSpec(new FingerTreeDeque[Int])
  with IntElements