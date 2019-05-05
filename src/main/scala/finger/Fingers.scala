package finger

import scala.language.higherKinds

object Fingers {

  // ==========================================================================================
  sealed trait FingerTree[+A]

  case object Empty extends FingerTree[Nothing]

  case class Single[A](a: A) extends FingerTree[A]

  case class Deep[A](l: Digit[A], down: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]

  // ==========================================================================================
  sealed trait Node[+A]

  case class Node2[A](a: A, b: A) extends Node[A]

  case class Node3[A](a: A, b: A, c: A) extends Node[A]

  // ==========================================================================================
  type Digit[A] = List[A]

  // ==========================================================================================
  trait Reduce[F[_]] {
    def reducer[A, B](fn: (A, B) => B): (F[A], B) => B

    def reducel[A, B](fn: (B, A) => B): (B, F[A]) => B
  }

  implicit val listReduce: Reduce[List] = new Reduce[List] {
    override def reducer[A, B](fn: (A, B) => B): (List[A], B) => B = (as, b) => as.foldRight(b)(fn)

    override def reducel[A, B](fn: (B, A) => B): (B, List[A]) => B = (b, as) => as.foldLeft(b)(fn)
  }

  def toList[A, F[_] : Reduce](s: F[A]): List[A] =
    implicitly[Reduce[F]].reducer[A, List[A]](_ :: _)(s, Nil)

  // ==========================================================================================
  implicit val nodeReduce: Reduce[Node] = new Reduce[Node] {
    override def reducer[A, B](fn: (A, B) => B): (Node[A], B) => B = (na, z) => na match {
      case Node2(a, b) => fn(a, fn(b, z))
      case Node3(a, b, c) => fn(a, fn(b, fn(c, z)))
    }

    override def reducel[A, B](fn: (B, A) => B): (B, Node[A]) => B = (z, na) => na match {
      case Node2(a, b) => fn(fn(z, b), a)
      case Node3(a, b, c) => fn(fn(fn(z, c), b), a)
    }
  }

  implicit val fingerTreeReduce: Reduce[FingerTree] = new Reduce[FingerTree] {
    override def reducer[A, B](fn: (A, B) => B): (FingerTree[A], B) => B = (fta, z) => fta match {
      case Empty => z
      case Single(a) => fn(a, z)
      case Deep(pr, m, sf) =>
        val fn1 = listReduce.reducer(fn)
        val fn2 = fingerTreeReduce.reducer(nodeReduce.reducer(fn))
        fn1(pr, fn2(m, fn1(sf, z)))
    }

    override def reducel[A, B](fn: (B, A) => B): (B, FingerTree[A]) => B = (z, fta) => fta match {
      case Empty => z
      case Single(a) => fn(z, a)
      case Deep(pr, m, sf) =>
        val fn1 = listReduce.reducel(fn)
        val fn2 = fingerTreeReduce.reducel(nodeReduce.reducel(fn))
        fn1(fn2(fn1(z, pr), m), sf)
    }
  }

  // ==========================================================================================
  def cons[A](a: A, ft: FingerTree[A]): FingerTree[A] = ft match {
    case Empty => Single(a)
    case Single(b) => Deep(List(a), Empty, List(b))
    case Deep(List(b, c, d, e), m, sf) => Deep(List(a, b), cons(Node3(c, d, e), m), sf)
    case Deep(pr, m, sf) => Deep(a :: pr, m, sf)
  }

  def snoc[A](ft: FingerTree[A], a: A): FingerTree[A] = ft match {
    case Empty => Single(a)
    case Single(b) => Deep(List(b), Empty, List(a))
    case Deep(pr, m, List(b, c, d, e)) => Deep(pr, snoc(m, Node3(e, d, c)), List(a, b))
    case Deep(pr, m, sf) => Deep(pr, m, a :: sf)
  }

  def cons1[A, F[_] : Reduce](fa: F[A], fta: FingerTree[A]): FingerTree[A] =
    implicitly[Reduce[F]].reducer[A, FingerTree[A]](cons)(fa, fta)

  def snoc1[A, F[_] : Reduce](fta: FingerTree[A], fa: F[A]): FingerTree[A] =
    implicitly[Reduce[F]].reducel[A, FingerTree[A]](snoc)(fta, fa)

  def toTree[A, F[_] : Reduce](fa: F[A]): FingerTree[A] =
    cons1(fa, Empty)

  // ==========================================================================================
  sealed trait ViewL[+S[_], +A]

  case object NilL extends ViewL[Nothing, Nothing]

  case class ConsL[+S[_], A](a: A, s: S[A]) extends ViewL[S, A]

  def viewL[A](fta: FingerTree[A]): ViewL[FingerTree, A] = fta match {
    case Empty => NilL
    case Single(a) => ConsL(a, Empty)
    case Deep(h :: t, m, sf) => ConsL(h, deepL(t, m, sf))
  }

  def deepL[A](pr: List[A], m: FingerTree[Fingers.Node[A]], sf: Fingers.Digit[A]): FingerTree[A] =
    pr match {
      case Nil => viewL(m) match {
        case NilL => toTree(sf.reverse)
        case ConsL(a, m1) => Deep(toList(a), m1, sf)
      }
      case _ => Deep(pr, m, sf)
    }

  // ==========================================================================================
  def isEmpty(ft: FingerTree[_]): Boolean =
    viewL(ft) match {
      case NilL => true
      case ConsL(_, _) => false
    }

  def head[A](ft: FingerTree[A]): A =
    viewL(ft) match {
      case NilL => sys.error("head() of an empty tree")
      case ConsL(h, _) => h
    }

  def tail[A](ft: FingerTree[A]): FingerTree[A] =
    viewL(ft) match {
      case NilL => sys.error("tail() of an empty tree")
      case ConsL(_, t) => t
    }
}
