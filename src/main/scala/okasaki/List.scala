package okasaki

import scala.collection.immutable.{List => ConsList}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait List[E] extends Stack[E, ConsList[E]] {

  override val empty: ConsList[E] = ConsList.empty

  override def cons(a: E, s: ConsList[E]): ConsList[E] = a :: s

  override def tail(s: ConsList[E]): ConsList[E] = s.tail

  override def isEmpty(s: ConsList[E]): Boolean = s.isEmpty

  override def head(s: ConsList[E]): E = s.head
}
