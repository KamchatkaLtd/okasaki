package okasaki

import scala.annotation.implicitNotFound
import scala.collection.immutable.{List => ConsList}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object Exercises {
  // ex 2.1
  @implicitNotFound("No member of type class Stack in scope for ${T}")
  def suffixes[E, ES, ESS](list: ES)(implicit ev: Stack[E, ES], evs: Stack[ES, ESS]): ESS =
    if (ev.isEmpty(list)) evs.cons(list, evs.empty)
    else evs.cons(list, suffixes(ev.tail(list)))

  // ex 2.5a
  def complete[E](x: E, d: Int): Tree[E] = d match {
    case 0 => Empty
    case dd if dd > 0 =>
      val tree = complete(x, dd - 1)
      SubTree(tree, x, tree)
  }

  // ex 2.5b
  def almostComplete[E](x: E, n: Int) = {
    def create2(x: E, m: Int): (Tree[E], Tree[E]) = m match {
      case 0 => (Empty, SubTree(Empty, x, Empty))
      case mm if mm > 0 =>
        val parts = create2(x, (mm - 1) / 2)
        if (mm % 2 == 1) (SubTree(parts._1, x, parts._1), SubTree(parts._1, x, parts._2))
        else (SubTree(parts._1, x, parts._2), SubTree(parts._2, x, parts._2))
    }

    create2(x, n)._1
  }


  def main (args: Array[String]) {
    implicit object ListOfInt extends List[Int]
    implicit object ListOfListOfInt extends List[ConsList[Int]]

    println(suffixes(ConsList(1,2,3)))
    println(complete("©", 3))
    println(almostComplete("∆", 4))
    println(almostComplete("∆", 3))
  }
}
