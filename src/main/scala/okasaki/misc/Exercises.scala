package okasaki.misc

/**
 * Copyright (C) 2015-2019 Kamchatka Ltd
 */
object Exercises {

  import okasaki.misc.BinaryTree._

  // ex 2.1
  def suffixes[E](list: List[E]): List[List[E]] =
    if (list.isEmpty) List(List())
    else list :: suffixes(list.tail)

  // ex 2.5a
  def complete[E](x: E, d: Int): BinaryTree[E] = d match {
    case 0 => Empty
    case dd if dd > 0 =>
      val tree = complete(x, dd - 1)
      SubBinaryTree(tree, x, tree)
  }

  // ex 2.5b
  def almostComplete[E](x: E, n: Int): BinaryTree[E] = {
    def create2(x: E, m: Int): (BinaryTree[E], BinaryTree[E]) = m match {
      case 0 => (Empty, SubBinaryTree(Empty, x, Empty))
      case mm if mm > 0 =>
        val parts = create2(x, (mm - 1) / 2)
        if (mm % 2 == 1) (SubBinaryTree(parts._1, x, parts._1), SubBinaryTree(parts._1, x, parts._2))
        else (SubBinaryTree(parts._1, x, parts._2), SubBinaryTree(parts._2, x, parts._2))
    }

    create2(x, n)._1
  }


  sealed trait Seq[+E]

  object Nil1 extends Seq[Nothing] {
    override def toString = s"Nil1"
  }

  case class Cons1[E](x: E, rest: Seq[(E, E)]) extends Seq[E]


  def main(args: Array[String]) {
    val ints = List(1, 2, 3)
    println(suffixes(ints).reverse)
    println(complete("©", 3))
    println(almostComplete("∆", 4))
    println(almostComplete("∆", 3))

    println(Cons1(1, Cons1((2, 3), Cons1(((4, 5), (6, 7)), Nil1))))
  }
}
