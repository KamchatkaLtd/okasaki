package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
sealed trait Tree[+E]

object Empty extends Tree[Nothing] {
  override def toString: String = "E"
}

case class SubTree[+E](left: Tree[E], elem: E, right: Tree[E]) extends Tree[E] {
  override def toString: String = s"T($left,$elem,$right)"
}
