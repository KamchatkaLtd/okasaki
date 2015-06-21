package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait List[E] extends Stack[E, ConsList[E]] {

  import okasaki.ConsList._

  override val empty: ConsList[E] = Empty

  override def cons(a: E, s: ConsList[E]): ConsList[E] = Cons(a, s)

  override def tail(s: ConsList[E]): ConsList[E] = s match {
    case Cons(h, t) => t
    case Empty => throw new IllegalArgumentException("tail called for an empty list")
  }

  override def isEmpty(s: ConsList[E]): Boolean = s == Empty

  override def head(s: ConsList[E]): E = s match {
    case Cons(h, t) => h
    case Empty => throw new IllegalArgumentException("tail called for an empty list")
  }
}
