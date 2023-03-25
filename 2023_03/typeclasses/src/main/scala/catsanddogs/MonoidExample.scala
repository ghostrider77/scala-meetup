package catsanddogs

object MonoidExample extends App {
  import cats.Monoid

//  implicit val intProdMonoid: Monoid[Int] = new Monoid[Int] {
//    def empty: Int = 1
//    def combine(x: Int, y: Int): Int = x * y
//  }

  def combineAll[T: Monoid](xs: List[T]): T =
    xs.foldLeft(Monoid[T].empty)(Monoid[T].combine)


  val x: Int = 3
  val res1: Int = Monoid[Int].combine(x, Monoid[Int].empty)
  val res2: Int = Monoid[Int].combine(Monoid[Int].empty, x)

  val lst: List[Int] = List(1, 2, 3, 4)
  val res3: Int = combineAll(lst)

  val xs: List[String] = List("I", " ", "like", " ", "cats.")
  val res4: String = combineAll(xs)

  println(res1)
  println(res2)

  println(res3)
  println(res4)
}
