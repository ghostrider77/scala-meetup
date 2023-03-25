package examples

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](x: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)

  // This is not necessary, but useful: basically an fmap with a function with 2 variables
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap(map(fa)(f.curried))(fb)
}

object Applicative {
  def apply[F[_]](implicit instance: Applicative[F]): Applicative[F] = instance

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](x: A): List[A] = List(x)

    def ap[A, B](fs: List[A => B])(xs: List[A]): List[B] = fs.flatMap(xs.map)
  }

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](x: A): Option[A] = Some(x)

    def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff match {
      case None => None
      case Some(f) => fa.map(f)
    }
  }
}

object ApplicativeExample extends App {
  val fs: List[Int => Int] = List(x => 2 * x, x => x + 10)
  val xs: List[Int] = List(1, 2, 3)
  val res0: List[Int] = Applicative[List].ap(fs)(xs)
  println(res0)

  val maybeFunc: Option[Int => String] = Some(x => s"$x is a number.")
  val res1: Option[String] = Applicative[Option].ap(maybeFunc)(Some(10))
  println(res1)

  val res2: Option[String] = Applicative[Option].ap(None)(Some(10))
  println(res2)

  def sum(x: Int)(y: Int): Int = x + y
  val sumContext: List[Int => Int => Int] = Applicative[List].pure(sum)
  val partiallyAppliedSum: List[Int => Int] = Applicative[List].ap(sumContext)(List(1, 2))

  val res3: List[Int] = Applicative[List].ap(partiallyAppliedSum)(List(10, 20))
  println(res3)

  val res4: List[Int] = Applicative[List].map2(List(1, 2), List(10, 20))(Function.uncurried(sum))
  println(res4)

  val res5: Option[Int] = Applicative[Option].map2(Some(1), Some(2))(_ + _)
  println(res5)
}
