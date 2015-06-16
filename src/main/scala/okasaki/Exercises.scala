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


  def main (args: Array[String]) {
    implicit object ListOfInt extends List[Int]
    implicit object ListOfListOfInt extends List[ConsList[Int]]

    println(suffixes(ConsList(1,2,3)))
  }
}
