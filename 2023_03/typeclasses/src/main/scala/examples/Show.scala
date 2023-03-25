package examples

trait Show[T] {
  def show(x: T): String
}

object Show {
  implicit val intShow: Show[Int] = new Show[Int] {
    def show(n: Int): String = s"Int($n)"
  }

  implicit val stringShow: Show[String] = new Show[String] {
    def show(s: String): String = s"""String("$s")"""
  }
}


object ShowExample {
     // using implicit showable instance
//   def show[T](x: T)(implicit instance: Show[T]): String =
//     instance.show(x)

    // alternative definition
  def show[T: Show](x: T): String =
    implicitly[Show[T]].show(x)


  def main(args: Array[String]): Unit = {
    println(show(100))
    println(show("Hello"))
    println(show(new Person("Joe", 30)))
    // println(show(1L))
  }
}
