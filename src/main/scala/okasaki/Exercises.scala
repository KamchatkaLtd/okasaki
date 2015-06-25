package okasaki

import scala.annotation.implicitNotFound

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object Exercises {
  import BinaryTree._
  import ConsList.reverse

  // ex 2.1
  @implicitNotFound("No member of type class Stack in scope for ${E}, ${ES}, ${ESS}")
  def suffixes[E, ES, ESS](list: ES)(implicit ev: Stack[E, ES], evs: Stack[ES, ESS]): ESS =
    if (ev.isEmpty(list)) evs.cons(list, evs.empty)
    else evs.cons(list, suffixes(ev.tail(list)))

  // ex 2.5a
  def complete[E](x: E, d: Int): BinaryTree[E] = d match {
    case 0 => Empty
    case dd if dd > 0 =>
      val tree = complete(x, dd - 1)
      SubBinaryTree(tree, x, tree)
  }

  // ex 2.5b
  def almostComplete[E](x: E, n: Int) = {
    def create2(x: E, m: Int): (BinaryTree[E], BinaryTree[E]) = m match {
      case 0 => (Empty, SubBinaryTree(Empty, x, Empty))
      case mm if mm > 0 =>
        val parts = create2(x, (mm - 1) / 2)
        if (mm % 2 == 1) (SubBinaryTree(parts._1, x, parts._1), SubBinaryTree(parts._1, x, parts._2))
        else (SubBinaryTree(parts._1, x, parts._2), SubBinaryTree(parts._2, x, parts._2))
    }

    create2(x, n)._1
  }


  def main (args: Array[String]) {
    implicit object ListOfInt extends List[Int]
    implicit object ListOfListOfInt extends List[ConsList[Int]]

    println(reverse(suffixes(Cons(1, Cons(2, Cons(3, ConsList.Empty))).asInstanceOf[ConsList[Int]])))
    println(complete("©", 3))
    println(almostComplete("∆", 4))
    println(almostComplete("∆", 3))
  }
}
