package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object Tree {
  object Empty extends Tree[Nothing] {
    override def toString: String = "E"
  }
}

sealed trait Tree[+E]

case class SubTree[+E](left: Tree[E], elem: E, right: Tree[E]) extends Tree[E] {
  override def toString: String = s"T($left,$elem,$right)"
}
