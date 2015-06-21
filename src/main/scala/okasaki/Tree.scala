package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object Tree {

  object Empty extends Tree[Nothing] {
    override def toString: String = "E"

    override val size = 0L
  }

}

sealed trait Tree[+E] {
  def size: Long
}

case class SubTree[+E](left: Tree[E], elem: E, right: Tree[E]) extends Tree[E] {
  override def toString: String = s"T($left,$elem,$right)"

  override lazy val size = 1L + left.size + right.size
}
