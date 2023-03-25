package catsanddogs

object MonadExample extends App {
  import cats.Monad

  def pair[A, B, M[_]](xs: M[A], ys: M[B])(implicit m: Monad[M]): M[(A, B)] =
    m.flatMap(xs)(x => m.flatMap(ys)(y => m.pure(x, y)))

  val res0: List[(Int, String)] = pair(List(1, 2), List("a", "b", "c"))
  println(res0)

  val res1: Option[(Int, String)] = pair(Some(1), Some("a"))
  println(res1)

  val res2: Option[(Int, String)] = pair(None, Some("a"))
  println(res2)

  val res3: Option[(Int, String)] = pair(Some(1), None)
  println(res3)
}
