package okasaki.misc

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object Misc {

  def log2(x: Int): Int =
    if (x <= 1) 0
    else Stream.iterate(1)(_ << 1).takeWhile(_ <= x).length - 1

}
