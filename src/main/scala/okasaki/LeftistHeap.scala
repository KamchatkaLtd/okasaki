package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object LeftistHeap {

  object Empty extends LeftistHeap[Nothing] {
    override def toString: String = "E"
  }

}

sealed trait LeftistHeap[+E]

case class SubHeap[E](rank: Int, x: E, left: LeftistHeap[E], right: LeftistHeap[E]) extends LeftistHeap[E] {
  override def toString: String = s"T($rank,$x,$left,$right)"
}
