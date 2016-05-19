package okasaki.maps

import okasaki.maps.TrieOfTrees._
import okasaki.maps.TrieOfTreesSpec._
import okasaki.maps.TrieSpec._
import okasaki.{FiniteMap, FiniteMapSpec}
import org.scalacheck.{Arbitrary, Gen}


object TrieOfTreesSpec {

  def insert[A](x: A, t: Tree[A])(implicit ord: Ordering[A]): Tree[A] = {
    def tryInsert(x: A, t: Tree[A]): Option[Tree[A]] = t match {
      case E => Some(T(x, E, E))
      case T(y, l, r) if ord.lt(x, y) => tryInsert(x, l) map (l1 => T(y, l1, r))
      case T(y, l, r) if ord.gt(x, y) => tryInsert(x, r) map (r1 => T(y, l, r1))
      case _ => None
    }

    tryInsert(x, t) getOrElse t
  }

  def treeOf[A: Ordering](gen: Gen[A]) =
    Gen.listOf(gen) map (_.foldLeft[Tree[A]](E)((t, x) => insert(x, t)))

}

class TrieOfTreesSpec extends FiniteMapSpec[Tree[Char], Int, Trie[Char, Int, Map]] {
  override implicit def keys: Arbitrary[Tree[Char]] = Arbitrary(treeOf(Gen.alphaNumChar))

  override implicit def elements: Arbitrary[Int] = Arbitrary.arbInt

  override def map = new TrieOfTrees[Char, Int, Map] {
    override def map = new FiniteMapLike[Map] {
      override def ops[K, V]: FiniteMap[K, V, Map[K, V]] = new ScalaFiniteMap[K, V]()
    }
  }
}
