package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Set[E] {
  def empty: Set[E]

  def insert(e: E): Set[E]

  def member(e: E): Boolean
}
