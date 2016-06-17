package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object Set {

  implicit class SetOps[E, Repr](s: Repr)(implicit set: Set[E, Repr]) {
    def insert(e: E): Repr = set.insert(e, s)

    def member(e: E): Boolean = set.member(e, s)
  }

}

trait Set[E, Repr] {
  def empty: Repr

  def insert(e: E, s: Repr): Repr

  def member(e: E, s: Repr): Boolean
}
