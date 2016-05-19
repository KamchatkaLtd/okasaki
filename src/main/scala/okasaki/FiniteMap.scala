package okasaki

/**
 * Fig. 2.10 and Fig 10.9
 *
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
object FiniteMap {

  class NotFound[T](value: T) extends RuntimeException

}

trait FiniteMap[K, V, M] {

  import okasaki.FiniteMap._

  def empty: M

  def bind(k: K, v: V, m: M): M

  @throws[NotFound[K]]
  def lookup(k: K, m: M): V
}

trait StringMap[V, M] extends FiniteMap[String, V, M]
