package examples


trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }
}

object FunctorExample {

  def fmap[A, B, F[_]](fa: F[A])(f: A => B)(implicit instance: Functor[F]): F[B] =
    instance.map(fa)(f)

  val missingOpt: Option[Int] = None
  val maybeTen: Option[Int] = Some(10)
  val stringList: List[String] = List("Hello", "world")
  val tree: BinaryTree[Long] = Node(Node(Leaf, 1, Leaf), 10, Node(Leaf, 20, Node(Leaf, 30, Leaf)))

  def main(args: Array[String]): Unit = {
    println(fmap(missingOpt)(_ / 2))
    println(fmap(maybeTen)(_ * 2))
    println(fmap(stringList)(_.reverse))
    println(fmap(tree)(_ * 2))
  }
}
