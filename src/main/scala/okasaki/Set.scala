package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Set[E, S] {
  def empty: S

  def insert(e: E, s: S): S

  def member(e: E, s: S): Boolean
}
