package examples

trait Monad3[F[_]] {
  // Everything can be defined using `pure`, `join` and `map`

  def pure[A](x: A): F[A]

  def join[A](ffa: F[F[A]]): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(map(fa))
}

object Monad3 {
  def apply[F[_]](implicit instance: Monad3[F]): Monad3[F] = instance

  implicit val listMonad: Monad3[List] = new Monad3[List] {
    def pure[A](x: A): List[A] = List(x)

    def join[A](ffa: List[List[A]]): List[A] = ffa.flatten

    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}


object Monad3Example extends App {
  val res0: List[Int] = Monad3[List].flatMap(List(1, 2, 3))(x => List(x, -x))
  println(res0)

  val res1: List[Int] = Monad3[List].join(List(List(1, 2), List(3)))
  println(res1)

  val res2: List[Int] = Monad3[List].pure(1)
  println(res2)

  val res3: List[Int] = Monad3[List].map(List(1, 2, 3))(_ + 1)
  println(res3)

  val res4: List[Int] = Monad3[List].ap(List((x: Int) => x + 1, (x: Int) => 10*x))(List(1, 2, 3))
  println(res4)
}
