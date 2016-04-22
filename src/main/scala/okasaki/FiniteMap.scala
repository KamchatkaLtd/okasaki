package okasaki

/**
 * Fig. 2.10 and Fig 10.9
 *
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object FiniteMap {
  class NotFound[T](value: T) extends RuntimeException
}

trait FiniteMap[K, V] {
  import FiniteMap._

  def empty: FiniteMap[K, V]

  def bind(k: K, v: V): FiniteMap[K, V]

  @throws[NotFound[K]]
  def lookup(k: K): V
}

trait StringMap[V] extends FiniteMap[String, V]
