package okasaki

import org.scalacheck.{Gen, Arbitrary}

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class LeftistHeapSpec extends HeapSpec {
  override type E = Int

  override type H = LeftistHeap[E]

  override implicit def elements: Arbitrary[E] = Arbitrary(Gen.chooseNum(Int.MinValue, Int.MaxValue, 0, 1, -1))

  override def heap: Heap[E, H] = new LeftistHeapOps[Int]()

  implicit val order = new Ordered[E] {
    override def eq(a: E, b: E): Boolean = a == b

    override def leq(a: E, b: E): Boolean = a <= b

    override def lt(a: E, b: E): Boolean = a < b
  }
}
