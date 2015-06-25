package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object BinaryTree {

  object Empty extends BinaryTree[Nothing] {
    override def toString: String = "E"

    override val size = 0L
  }

}

sealed trait BinaryTree[+E] {
  def size: Long
}

case class SubBinaryTree[+E](left: BinaryTree[E], elem: E, right: BinaryTree[E]) extends BinaryTree[E] {
  override def toString: String = s"T($left,$elem,$right)"

  override lazy val size = 1L + left.size + right.size
}
