package examples

object Examples extends App {
  // SequenceA transforms a list of applicative actions into a single action that returns a list of result values.

  def sequenceA[A, F[_]](xs: List[F[A]])(implicit instance: Applicative[F]): F[List[A]] = xs match {
    case Nil => instance.pure(Nil)
    case x :: xss => instance.map2(x, sequenceA(xss))(_ :: _)
  }

  def replicateM[A, M[_]](n: Int, ma: M[A])(implicit instance: Monad[M]): M[List[A]] =
    sequenceA(List.fill(n)(ma))

  val xs: List[Option[Int]] = List(Some(1), Some(3))
  // val xs: List[Option[Int]] = List(Some(1), None, Some(3))
  // val xs: List[List[String]] = List(List("x", "y"), List("a", "b", "c"))
  // val xs: List[List[String]] = List(Nil, List("a", "b", "c"))
  println(sequenceA(xs))

//  println(replicateM(5, List("a")))
//  println(replicateM(3, Some(10): Option[Int]))
//  println(replicateM(10, None))

}
