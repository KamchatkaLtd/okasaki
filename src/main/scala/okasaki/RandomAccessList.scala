package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
object RandomAccessList {

  case class Subscript() extends IndexOutOfBoundsException

}

trait RandomAccessList[E, RL] {
  def empty: RL

  def isEmpty: RL => Boolean

  def cons: (RL, E) => RL

  def head: RL => E

  def tail: RL => RL

  def lookup: (Int, RL) => E

  def update: (Int, E, RL) => RL
}
