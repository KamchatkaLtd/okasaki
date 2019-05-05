package finger

import finger.Fingers.Digit.digitReduce
import finger.Fingers.Node.nodeReduce

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

  object Node {
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
  }

  // ==========================================================================================
  sealed trait Digit[+A]

  case class One[A](a: A) extends Digit[A]

  case class Two[A](a: A, b: A) extends Digit[A]

  case class Three[A](a: A, b: A, c: A) extends Digit[A]

  case class Four[A](a: A, b: A, c: A, d: A) extends Digit[A]

  object Digit {
    def cons[A](x: A, sf: Digit[A]): Digit[A] = sf match {
      case One(a) => Two(x, a)
      case Two(a, b) => Three(x, a, b)
      case Three(a, b, c) => Four(x, a, b, c)
      case Four(_, _, _, _) => sys.error("cons onto Four")
    }

    def snoc[A](sf: Digit[A], x: A): Digit[A] = sf match {
      case One(a) => Two(a, x)
      case Two(a, b) => Three(a, b, x)
      case Three(a, b, c) => Four(a, b, c, x)
      case Four(_, _, _, _) => sys.error("snoc onto Four")
    }

    implicit val digitReduce: Reduce[Digit] = new Reduce[Digit] {
      override def reducer[A, B](fn: (A, B) => B): (Digit[A], B) => B =
        (as, z) => as match {
          case One(a) => fn(a, z)
          case Two(a, b) => fn(a, fn(b, z))
          case Three(a, b, c) => fn(a, fn(b, fn(c, z)))
          case Four(a, b, c, d) => fn(a, fn(b, fn(c, fn(d, z))))
        }

      override def reducel[A, B](fn: (B, A) => B): (B, Digit[A]) => B =
        (z, as) => as match {
          case One(a) => fn(z, a)
          case Two(a, b) => fn(fn(z, a), b)
          case Three(a, b, c) => fn(fn(fn(z, a), b), c)
          case Four(a, b, c, d) => fn(fn(fn(fn(z, a), b), c), d)
        }
    }
  }

  // ==========================================================================================
  trait Reduce[F[_]] {
    def reducer[A, B](fn: (A, B) => B): (F[A], B) => B

    def reducel[A, B](fn: (B, A) => B): (B, F[A]) => B
  }

  object Reduce {
    implicit val listReduce: Reduce[List] = new Reduce[List] {
      override def reducer[A, B](fn: (A, B) => B): (List[A], B) => B = (as, b) => as.foldRight(b)(fn)

      override def reducel[A, B](fn: (B, A) => B): (B, List[A]) => B = (b, as) => as.foldLeft(b)(fn)
    }
  }

  def toList[A, F[_] : Reduce](s: F[A]): List[A] =
    implicitly[Reduce[F]].reducer[A, List[A]](_ :: _)(s, Nil)

  // ==========================================================================================

  implicit val fingerTreeReduce: Reduce[FingerTree] = new Reduce[FingerTree] {
    override def reducer[A, B](fn: (A, B) => B): (FingerTree[A], B) => B = (fta, z) => fta match {
      case Empty => z
      case Single(a) => fn(a, z)
      case Deep(pr, m, sf) =>
        val fn1 = digitReduce.reducer(fn)
        val fn2 = fingerTreeReduce.reducer(nodeReduce.reducer(fn))
        fn1(pr, fn2(m, fn1(sf, z)))
    }

    override def reducel[A, B](fn: (B, A) => B): (B, FingerTree[A]) => B = (z, fta) => fta match {
      case Empty => z
      case Single(a) => fn(z, a)
      case Deep(pr, m, sf) =>
        val fn1 = digitReduce.reducel(fn)
        val fn2 = fingerTreeReduce.reducel(nodeReduce.reducel(fn))
        fn1(fn2(fn1(z, pr), m), sf)
    }
  }

  // ==========================================================================================
  def cons[A](x: A, ft: FingerTree[A]): FingerTree[A] = ft match {
    case Empty => Single(x)
    case Single(a) => Deep(One(x), Empty, One(a))
    case Deep(Four(a, b, c, d), m, sf) => Deep(Two(x, a), cons(Node3(b, c, d), m), sf)
    case Deep(pr, m, sf) => Deep(Digit.cons(x, pr), m, sf)
  }

  def snoc[A](ft: FingerTree[A], x: A): FingerTree[A] = ft match {
    case Empty => Single(x)
    case Single(a) => Deep(One(a), Empty, One(x))
    case Deep(pr, m, Four(a, b, c, d)) => Deep(pr, snoc(m, Node3(a, b, c)), Two(d, x))
    case Deep(pr, m, sf) => Deep(pr, m, Digit.snoc(sf, x))
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
    case Deep(One(a), m, sf) => ConsL(a, deepL(m, sf))
    case Deep(Two(a, b), m, sf) => ConsL(a, Deep(One(b), m, sf))
    case Deep(Three(a, b, c), m, sf) => ConsL(a, Deep(Two(b, c), m, sf))
    case Deep(Four(a, b, c, d), m, sf) => ConsL(a, Deep(Three(b, c, d), m, sf))
  }

  def deepL[A](m: FingerTree[Node[A]], sf: Digit[A]): FingerTree[A] =
    viewL(m) match {
      case NilL => toTree(sf)
      case ConsL(Node2(a, b), m1) => Deep(Two(a, b), m1, sf)
      case ConsL(Node3(a, b, c), m1) => Deep(Three(a, b, c), m1, sf)
    }

  // ==========================================================================================
  sealed trait ViewR[+S[_], +A]

  case object NilR extends ViewR[Nothing, Nothing]

  case class SnocR[+S[_], A](s: S[A], a: A) extends ViewR[S, A]

  def viewR[A](fta: FingerTree[A]): ViewR[FingerTree, A] = fta match {
    case Empty => NilR
    case Single(a) => SnocR(Empty, a)
    case Deep(pr, m, One(a)) => SnocR(deepR(pr, m), a)
    case Deep(pr, m, Two(a, b)) => SnocR(Deep(pr, m, One(a)), b)
    case Deep(pr, m, Three(a, b, c)) => SnocR(Deep(pr, m, Two(a, b)), c)
    case Deep(pr, m, Four(a, b, c, d)) => SnocR(Deep(pr, m, Three(a, b, c)), d)
  }

  def deepR[A](pr: Digit[A], m: FingerTree[Fingers.Node[A]]): FingerTree[A] =
    viewR(m) match {
      case NilR => toTree(pr)
      case SnocR(m1, Node2(a, b)) => Deep(pr, m1, Two(a, b))
      case SnocR(m1, Node3(a, b, c)) => Deep(pr, m1, Three(a, b, c))
    }

  // ==========================================================================================
  def isEmpty(ft: FingerTree[_]): Boolean =
    viewL(ft) match {
      case NilL => true
      case ConsL(_, _) => false
    }

  def head[A](ft: FingerTree[A]): A =
    viewL(ft) match {
      case NilL => throw new IllegalStateException("head() of an empty tree")
      case ConsL(h, _) => h
    }

  def tail[A](ft: FingerTree[A]): FingerTree[A] =
    viewL(ft) match {
      case NilL => throw new IllegalStateException("tail() of an empty tree")
      case ConsL(_, t) => t
    }

  def last[A](ft: FingerTree[A]): A =
    viewR(ft) match {
      case NilR => throw new IllegalStateException("last() of an empty tree")
      case SnocR(_, h) => h
    }

  def init[A](ft: FingerTree[A]): FingerTree[A] =
    viewR(ft) match {
      case NilR => throw new IllegalStateException("init() of an empty tree")
      case SnocR(t, _) => t
    }
}
