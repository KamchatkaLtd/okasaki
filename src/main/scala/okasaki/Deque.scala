package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Deque[E, Q] extends OutputRestrictedDeque[E, Q] {
  def init: Q => Q

  def last: Q => E
}
