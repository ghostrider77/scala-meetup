package catsanddogs

object FoldableExample extends App {
  import cats.Foldable

  val res0: String = Foldable[List].fold(List("a", "b", "c"))
  println(res0)
  
}
