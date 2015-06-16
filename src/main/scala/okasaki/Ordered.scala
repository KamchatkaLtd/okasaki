package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Ordered[E] {
  def eq(a: E, b: E): Boolean

  def lt(a: E, b: E): Boolean

  def leq(a: E, b: E): Boolean
}
