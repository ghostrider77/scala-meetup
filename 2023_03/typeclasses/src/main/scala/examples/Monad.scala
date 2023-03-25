package examples


trait Monad[F[_]] extends Applicative[F] {
  // a Monad is defined as an Applicative (`pure` and `ap`, hence `map` is already defined), endowed with `flatMap`

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad {
  def apply[F[_]](implicit instance: Monad[F]): Monad[F] = instance

  implicit val listMonad: Monad[List] = new Monad[List] {
    def pure[A](x: A): List[A] = List(x)

    def ap[A, B](fs: List[A => B])(fa: List[A]): List[B] = fs.flatMap(fa.map)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff.flatMap(fa.map)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }
}


object MonadExample extends App {
  def pairs[A, B](xs: List[A], ys: List[B]): List[(A, B)] =
    Monad[List].flatMap(xs)(x => Monad[List].flatMap(ys)(y => Monad[List].pure(x, y)))

  val res0: List[(Int, String)] = pairs(List(1, 2), List("a", "b", "c"))
  println(res0)

  val res1: Option[String] = Monad[Option].flatMap(Some(1))(x => Some(s"My value is $x."))
  println(res1)

  val res2: List[Int] = Monad2[List].flatMap(List(1, 2, 3))(x => List(x, -x))
  println(res2)

  val res3: List[Int] = Monad2[List].join(List(List(1, 2), List(3)))
  println(res3)

  val res4: List[Int] = Monad2[List].pure(1)
  println(res4)

  val res5: List[Int] = Monad2[List].map(List(1, 2, 3))(_ + 1)
  println(res5)

  val res6: List[Int] = Monad2[List].ap(List((x: Int) => x + 1, (x: Int) => 10 * x))(List(1, 2, 3))
  println(res6)

}
