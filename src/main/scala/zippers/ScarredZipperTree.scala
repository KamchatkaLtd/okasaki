package zippers

/**
 * The zipper idea as presented in
 * <a href="http://dl.acm.org/citation.cfm?id=969867.969872">FUNCTIONAL PEARL: The Zipper</a>
 * by Gérard Huet
 *
 * J. Functional Programming 7 (5): 549–554, September 1997. Printed in the United Kingdom 549
 * (c) 1997 Cambridge University Press
 *
 * Scala translation (c) 2015 Kamchatka Ltd
 */
object ScarredZipperTree {

  sealed trait Tree[E]

  case class Item[E](item: E) extends Tree[E]

  case class Siblings[E](l: List[Tree[E]], t: Tree[E], r: List[Tree[E]]) extends Tree[E]


  sealed trait Path[+E]

  object Top extends Path[Nothing]

  case class Node[E](l: List[Tree[E]], p: Path[E], r: List[Tree[E]]) extends Path[E]


  case class Loc[E](t: Tree[E], p: Path[E]) {

    def go_left: Loc[E] = p match {
      case Top => failwith("left of top")
      case Node(l :: left, up, right) => Loc(l, Node(left, up, t :: right))
      case _ => failwith("left of first")
    }

    def go_right: Loc[E] = p match {
      case Top => failwith("right of top")
      case Node(left, up, r :: right) => Loc(r, Node(t :: left, up, right))
      case _ => failwith("right of last")
    }

    def go_up: Loc[E] = p match {
      case Top => failwith("up of top")
      case Node(left, up, right) => Loc(Siblings(left, t, right), up)
    }

    def go_down: Loc[E] = t match {
      case Item(_) => failwith("down of item")
      case Siblings(left, t1, right) => Loc(t1, Node(left, p, right))
      case _ => failwith("down of empty")
    }

    def change(t1: Tree[E]): Loc[E] = Loc(t1, p)

    def insert_right(r: Tree[E]): Loc[E] = p match {
      case Top => failwith("insert of top")
      case Node(left, up, right) => Loc(t, Node(left, up, r :: right))
    }

    def insert_left(l: Tree[E]): Loc[E] = p match {
      case Top => failwith("insert of top")
      case Node(left, up, right) => Loc(t, Node(l :: left, up, right))
    }

    def insert_down(t1: Tree[E]): Loc[E] = t match {
      case Item(_) => failwith("down of item")
      case Siblings(left, up, right) => Loc(Siblings(left, up, t1 :: right), p)
    }

    def delete: Loc[E] = p match {
      case Top => failwith("delete of top")
      case Node(left, up, r :: right) => Loc(r, Node(left, up, right))
      case Node(l :: left, up, Nil) => Loc(l, Node(left, up, Nil))
      case Node(Nil, up, Nil) => Loc(t, up).delete
    }
  }

  def failwith(m: String): Nothing = throw new IllegalStateException(m)
}
