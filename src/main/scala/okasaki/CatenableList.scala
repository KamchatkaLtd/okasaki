package okasaki

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
trait CatenableList[E, CL] {

  def empty: CL

  def isEmpty: CL => Boolean

  def cons: (E, CL) => CL

  def snoc: (CL, E) => CL

  def ++ : (CL, CL) => CL

  def head: CL => E

  def tail: CL => CL
}
