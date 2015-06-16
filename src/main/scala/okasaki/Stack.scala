package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */

trait Stack[E, StackE] {

  def empty: StackE

  def isEmpty(s: StackE): Boolean
  def cons(a: E, s: StackE): StackE
  def head(s: StackE): E
  def tail(s: StackE): StackE
}
