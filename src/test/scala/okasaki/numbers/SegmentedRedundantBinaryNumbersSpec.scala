package okasaki.numbers

import okasaki.numbers.SegmentedRedundantBinaryNumbers._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class SegmentedRedundantBinaryNumbersSpec extends Specification with ScalaCheck {
  "Segmented redundant binary numbers" should {
    "have reasonable values" in {
      List(Ones(8)).asLong === 255
      List(Threes(2)).asLong === 9
      List(Zero, Ones(1)).asLong === 2
      List(Zero, Zero, Ones(1)).asLong === 4
      List(Zero, Zero, Zero, Zero, Ones(1)).asLong === 16
      List(Zero, Four).asLong === 8
      List(Ones(2), Zero, Ones(1)).asLong === 11
    }

    "be easy to create" >> {

      "simple examples" >> {
        toNumber(0) === Nil
        toNumber(1) === List(Ones(1))
        toNumber(2) === List(Two)
        toNumber(3) === List(Threes(1))
        toNumber(4) === List(Two, Ones(1))
        toNumber(9) === List(Threes(1), Ones(2))
        toNumber(1023) === List(Threes(1), Two, Two, Two, Two, Two, Two, Two, Two)
      }

      "toNumber is correct" ! prop {
        (x: Int) => (x >= 0) ==>
          (toNumber(x).asLong === x)
      }
    }

    "support increment" in {
      "inc(a) == a + 1" ! prop { (a: Int) => (a >= 0) ==>
        (inc(toNumber(a)).asLong === a + 1L)
      }

      "simple examples" >> {
        inc(toNumber(0)) === List(Ones(1))
        inc(toNumber(1)) === List(Two)
        inc(toNumber(2)) === List(Threes(1))
        inc(toNumber(3)) === List(Two, Ones(1))
        inc(toNumber(4)) === List(Threes(1), Ones(1))
      }
    }

    "support decrement" in {
      "dec(a) == a + 1" ! prop { (a: Int) => (a > 0) ==>
        (dec(toNumber(a)).asLong === a - 1L)
      }

      "simple examples" >> {
        dec(toNumber(1)) === Nil
        dec(toNumber(2)) === List(Ones(1))
        dec(toNumber(3)) === List(Two)
        dec(toNumber(4)) === List(Ones(2))
        dec(toNumber(5)) === List(Two, Ones(1))
        dec(toNumber(6)) === List(Ones(1), Two)
      }
    }
  }

}
