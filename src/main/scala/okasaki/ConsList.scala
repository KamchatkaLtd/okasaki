package okasaki

import scala.annotation.tailrec

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object ConsList {

  object Empty extends ConsList[Nothing] {
    override def toString: String = "E"
  }

  @tailrec
  def foldl[A, B](l: ConsList[A], a: B, f: (A, B) => B): B = l match {
    case Empty => a
    case Cons(h, t) => foldl(t, f(h, a), f)
  }

  def rev_map[A, B](l: ConsList[A], a: ConsList[B], f: A => B): ConsList[B] =
    foldl(l, Empty, (a: A, l: ConsList[B]) => Cons(f(a), l))

  def reverse[A](l: ConsList[A]): ConsList[A] = rev_map(l, Empty, (x: A) => x)

  def map[A, B](l: ConsList[A], f: A => B): ConsList[B] = reverse(rev_map(l, Empty, f))
}

sealed trait ConsList[+E]

case class Cons[E](head: E, tail: ConsList[E]) extends ConsList[E]
