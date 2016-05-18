package okasaki

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
trait CatenableDeque[E, Q] extends Deque[E, Q] with CatenableList[E, Q]