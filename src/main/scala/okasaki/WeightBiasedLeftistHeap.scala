package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object WeightBiasedLeftistHeap {

  object Empty extends WeightBiasedLeftistHeap[Nothing] {
    override def toString: String = "E"
  }

  case class SubHeap[E](weight: Long,
                        x: E,
                        left: WeightBiasedLeftistHeap[E],
                        right: WeightBiasedLeftistHeap[E]) extends WeightBiasedLeftistHeap[E] {
    override def toString: String = s"T($weight,$x,$left,$right)"
  }
}

sealed trait WeightBiasedLeftistHeap[+E]
