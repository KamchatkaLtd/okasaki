package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
trait Sortable[E, S] {
  def ord: Ordering[E]

  def empty: S

  def add(e: E, s: S): S

  def sort(s: S): List[E]
}
