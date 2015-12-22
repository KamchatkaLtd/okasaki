package okasaki

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._

/**
 * Copyright (C) 2015 Kamchatka Ltd
 */
class HeapWithDeletionSpec
  extends HeapSpec(new HeapWithDeletion(new SkewBinomialHeap[Int]))
  with IntElements {

  "Heap with deletion" should {
    "support deletion" in {
      val heap = new HeapWithDeletion(new SkewBinomialHeap[Int])

      "single element" ! prop { lwe: ListWithElement =>
        val ListWithElement(l, x) = lwe
        val h = l.foldRight(heap.empty)(heap.insert)

        drain(heap.delete(x, h)) === deleteFirst(x, l.sorted)
      }
    }
  }


  def deleteFirst[T](e: T, xs: List[T]): List[T] = {
    val i = xs.indexOf(e)
    if (i == -1) xs else xs.take(i) ++ xs.drop(i + 1)
  }

  case class ListWithElement(l: List[Int], i: Int) {
    assert(l.contains(i))
  }

  lazy implicit val listWithElement: Arbitrary[ListWithElement] =
    Arbitrary(for {
      xs <- arbitrary[List[Int]]
      i <- choose(0, xs.size - 1)
    } yield ListWithElement(xs, xs(i)))
}
