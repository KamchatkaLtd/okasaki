package okasaki.heaps

import okasaki.{HeapSpec, HeapWithDeletion, IntElements}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._

/**
 * Copyright (C) 2015-2019 Kamchatka Ltd
 */
class HeapWithDeletionSpec
  extends HeapSpec[Int, HeapWithDeletion[Int, SkewBinomialHeap[Int]]]
    with IntElements {

  def empty = new HeapWithDeletion(new SkewBinomialHeap[Int]())

  "Heap with deletion" should {
    "support deletion" in {
      "single element" ! prop { lwe: ListWithElement =>
        val ListWithElement(l, x) = lwe
        val h = fromList(l)

        h.delete(x).toList === deleteFirst(x, l.sorted)
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
      if xs.nonEmpty
      i <- indexIn(xs)
    } yield ListWithElement(xs, xs(i)))

  private def indexIn(xs: List[Int]): Gen[Int] =
    choose(0, Math.max(0, xs.size - 1))
}
