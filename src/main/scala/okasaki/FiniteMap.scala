package okasaki

/**
 * Fig. 2.10
 *
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait FiniteMap[K, V, M] {
  class NotFound extends RuntimeException

  def empty: M

  def bind(k: K, v: V, s: M): M

  @throws[NotFound]
  def lookup(k: K, s: M): V
}
