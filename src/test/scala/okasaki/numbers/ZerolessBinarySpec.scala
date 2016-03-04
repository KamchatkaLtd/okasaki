package okasaki.numbers

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class ZerolessBinarySpec extends Specification with ScalaCheck {

  import okasaki.numbers.ZerolessBinary._

  "Zeroless numbers" should {
    "be reversible" ! prop {
      n: Int => (n >= 0) ==>
        (toLong(toZeroless(n)) == n)
    }

    "Support increment" ! prop {
      n: Int => (n >= 0) ==>
        (toLong(inc(toZeroless(n))) == n + 1l)
    }

    "Support decrement" ! prop {
      n: Int => (n > 0) ==>
        (toLong(dec(toZeroless(n))) == n - 1l)
    }

    "Support addition" ! prop {
      (a: Int, b: Int) => (a >= 0 && b >= 0) ==> {
        val sum = toLong(add(toZeroless(a), toZeroless(b)))
        sum == a.toLong + b.toLong
      }
    }
  }
}
