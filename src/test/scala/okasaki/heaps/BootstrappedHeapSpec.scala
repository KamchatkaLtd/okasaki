package okasaki.heaps

import okasaki.heaps.BootstrappedHeap.{BSHeap, Empty}
import okasaki.heaps.BootstrappedHeapSpec.BootstrappedHeapFromSkewBinomial
import okasaki.{HeapSpec, IntElements}

import scala.math.Ordering

/**
 * Copyright (C) 2016 Kamchatka Ltd
 */
object BootstrappedHeapSpec {

  class BootstrappedHeapFromSkewBinomial[E](h: BSHeap[E, SkewBinomialHeap] = Empty)(implicit val ord: Ordering[E])
    extends BootstrappedHeap[E, SkewBinomialHeap, BootstrappedHeapFromSkewBinomial[E]](h) {

    override def baseHeap =
      new SkewBinomialHeap[BSHeap[E, SkewBinomialHeap]]()

    override def create(h: BSHeap[E, SkewBinomialHeap]): BootstrappedHeapFromSkewBinomial[E] =
      new BootstrappedHeapFromSkewBinomial[E](h)
  }

}

class BootstrappedHeapSpec
  extends HeapSpec[Int, BootstrappedHeapFromSkewBinomial[Int]]
  with IntElements {

  def empty = new BootstrappedHeapFromSkewBinomial[Int]()
}
