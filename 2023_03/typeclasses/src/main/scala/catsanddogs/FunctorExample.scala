package catsanddogs

import cats.Functor
import scala.util.{Success, Try}

object FunctorExample extends App {
  import examples.{BinaryTree, Node, Leaf}

  implicit object TreeFunctor extends Functor[BinaryTree] {
    def map[A, B](fa: BinaryTree[A])(f: A => B): BinaryTree[B] = fa match {
      case Leaf => Leaf
      case Node(left, value, right) => Node(map(left)(f), f(value), map(right)(f))
    }
  }

  val lst: List[String] = List("Ann", "Bob")
  val res0: List[String] = Functor[List].map(lst)(name => "My name is " + name)
  val res1: List[String] = Functor[List].map(lst)(_.reverse)
  println(res0)
  println(res1)

  val tree: BinaryTree[Int] = Node(Node(Leaf, 1, Leaf), 10, Node(Leaf, 20, Leaf))
  val res3 = Functor[BinaryTree].map(tree)(_ * 2)
  println(res3)

  val res4 = Functor[BinaryTree].fmap(tree)(_ * 2)
  println(res4)
}
