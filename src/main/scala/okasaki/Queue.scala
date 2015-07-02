package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Queue[E, Q] {
  def empty: Q

  def isEmpty: Q => Boolean

  def snoc: (Q, E) => Q

  def head: Q => E

  def tail: Q => Q
}
