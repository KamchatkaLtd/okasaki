package okasaki

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
trait CatenableList[E, CL] {

  def empty: CL

  def isEmpty(cl: CL): Boolean

  def cons(e: E, cl: CL): CL

  def snoc(cl: CL, e: E): CL

  def ++(a: CL, b: CL): CL

  def head(cl: CL): E

  def tail(cl: CL): CL
}
