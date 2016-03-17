package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Queue[E, Q] {
  def empty: Q

  def isEmpty(q: Q): Boolean

  def snoc(q: Q, e: E): Q

  def head(q: Q): E

  def tail(q: Q): Q
}
