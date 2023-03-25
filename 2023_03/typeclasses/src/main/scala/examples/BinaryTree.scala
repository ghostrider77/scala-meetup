package examples


sealed trait BinaryTree[+T]

case object Leaf extends BinaryTree[Nothing]
case class Node[T](left: BinaryTree[T], value: T, right: BinaryTree[T]) extends BinaryTree[T]


object BinaryTree {
  implicit val treeFunctor: Functor[BinaryTree] = new Functor[BinaryTree] {
    def map[A, B](fa: BinaryTree[A])(f: A => B): BinaryTree[B] = fa match {
      case Leaf => Leaf
      case Node(left, value, right) => Node(map(left)(f), f(value), map(right)(f))
    }
  }
}
