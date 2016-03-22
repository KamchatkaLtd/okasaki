package okasaki

/**
 * Fig. 3.1
 *
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
trait Heap[E, H <: Heap[E, H]] {
  implicit def ord: Ordering[E]

  def empty: H

  def isEmpty: Boolean

  def insert(x: E): H

  def merge(b: H): H

  def findMin: E

  def deleteMin: H

  def toList: List[E] =
    if (isEmpty) Nil else findMin :: deleteMin.toList
}
