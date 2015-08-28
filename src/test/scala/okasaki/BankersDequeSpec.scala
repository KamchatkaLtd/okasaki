package okasaki

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class BankersDequeSpec
  extends DequeSpec(new BankersDeque[Int](3))
  with IntElements
