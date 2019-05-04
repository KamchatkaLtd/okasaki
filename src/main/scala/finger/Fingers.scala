package finger

import scala.language.higherKinds

object Fingers {

  // ==========================================================================================
  sealed trait FingerTree[+A]

  case object Empty extends FingerTree[Nothing]

  case class Single[A](a: A) extends FingerTree[A]

  case class Deep[A](l: Digit[A], down: FingerTree[Node[A]], r: Digit[A]) extends FingerTree[A]

  // ==========================================================================================
  sealed trait Node[A]

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
      case Deep(pr, m, sf) => ???
    }

    override def reducel[A, B](fn: (B, A) => B): (B, FingerTree[A]) => B = (z, fta) => fta match {
      case Empty => z
      case Single(a) => fn(z, a)
      case Deep(pr, m, sf) => ???
    }
  }
}
