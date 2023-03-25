package examples

trait Monad2[F[_]] {
  // Everything can be defined using `pure` and `flatMap`

  def pure[A](x: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((x: A) => pure(f(x)))

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(map(fa))
}

object Monad2 {
  def apply[F[_]](implicit instance: Monad2[F]): Monad2[F] = instance

  implicit val listMonad: Monad2[List] = new Monad2[List] {
    def pure[A](x: A): List[A] = List(x)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }
}


object Monad2Example extends App {
  val res0: List[Int] = Monad2[List].flatMap(List(1, 2, 3))(x => List(x, -x))
  println(res0)

  val res1: List[Int] = Monad2[List].join(List(List(1, 2), List(3)))
  println(res1)

  val res2: List[Int] = Monad2[List].pure(1)
  println(res2)

  val res3: List[Int] = Monad2[List].map(List(1, 2, 3))(_ + 1)
  println(res3)

  val res4: List[Int] = Monad2[List].ap(List((x: Int) => x + 1, (x: Int) => 10*x))(List(1, 2, 3))
  println(res4)
}
