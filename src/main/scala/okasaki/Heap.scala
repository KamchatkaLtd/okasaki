package okasaki

/**
 * Fig. 3.1
 *
 * Copyright (C) 2015-2016 Kamchatka Ltd
 */
trait Heap[E, H] {
  implicit def ord: Ordering[E]

  def empty: H

  def isEmpty(h: H): Boolean

  def insert(x: E, h: H): H

  def merge(a: H, b: H): H

  def findMin(h: H): E

  def deleteMin(h: H): H
}
