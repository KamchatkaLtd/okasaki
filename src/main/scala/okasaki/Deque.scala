package okasaki

/**
  * Copyright (C) 2015 Kamchatka Ltd
  */
trait Deque[E, Q] extends Queue[E, Q] {
   def cons: (Q, E) => Q

   def last: Q => E

   def init: Q => Q
 }
