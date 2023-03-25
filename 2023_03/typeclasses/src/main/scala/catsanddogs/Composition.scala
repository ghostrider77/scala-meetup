package catsanddogs

object Composition extends App {
  import cats.Functor

  val xs: List[Option[Int]] = List(Some(1), None, Some(3))
  val op: Option[List[Int]] = Some(List(1, 2, 3))

  // xs.map(_.map(_ * 2))
  // op.map(_.map(_ * 2))

  Functor[List]
    .compose(Functor[Option])
    .map(xs)(_ * 2)

  Functor[Option]
    .compose(Functor[List])
    .map(op)(_ * 2)

  def mapCompose[A, B, F[_] : Functor, G[_] : Functor](xs: F[G[A]])(f: A => B): F[G[B]] =
    Functor[F].compose(Functor[G]).map(xs)(f)


  println(mapCompose(xs)(_ * 2))
  println(mapCompose(op)(_ * 2))
  println(mapCompose(op)(x => s"This is $x"))
}
