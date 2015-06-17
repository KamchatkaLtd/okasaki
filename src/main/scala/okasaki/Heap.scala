package okasaki

/**
 * Fig. 3.1
 *
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Heap[E, H] {
  implicit def ord: Ordered[E]

  def empty: H

  def isEmpty: H => Boolean

  def insert: (E, H) => H

  def merge: (H, H) => H

  def findMin: H => E

  def deleteMin: H => H
}
