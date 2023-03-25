package catsanddogs

object ApplicativeExample extends App {
  import cats.Applicative

  val fs: List[Int => Int] = List(x => 2 * x, x => x + 10)
  val xs: List[Int] = List(1, 2, 3)
  val res0: List[Int] = Applicative[List].ap(fs)(xs)
  println(res0)

  val res1 = Applicative[List].<*>(fs)(xs)
  println(res1)

  def sum(x: Int)(y: Int): Int = x + y
  val sumContext: List[Int => Int => Int] = Applicative[List].pure(sum)
  val partiallyAppliedSum: List[Int => Int] = Applicative[List].ap(sumContext)(List(1, 2))
  val res2: List[Int] = Applicative[List].ap(partiallyAppliedSum)(List(10, 20))
  println(res2)

  val res3: Option[Int] = Applicative[Option].map2(Some(1), Some(2))(_ + _)
  println(res3)
}
