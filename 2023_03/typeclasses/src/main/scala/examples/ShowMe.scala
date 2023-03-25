package examples


trait ShowMe[T] {
  def show(x: T): String
}

object ShowMe {
  def apply[T](implicit instance: ShowMe[T]): ShowMe[T] = implicitly[ShowMe[T]]

  implicit val intShow: ShowMe[Int] = new ShowMe[Int] {
    def show(n: Int): String = s"Int($n)"
  }

  implicit val stringShow: ShowMe[String] = new ShowMe[String] {
    def show(s: String): String = s"""String("$s")"""
  }
}

object ShowMeExample {
  implicit val personShowMe: ShowMe[Person] = new ShowMe[Person] {
    def show(p: Person): String = s"Person(name: ${p.name}, age: ${p.age})"
  }

  def main(args: Array[String]): Unit = {
    println(ShowMe[Int].show(123))
    println(ShowMe[String].show("Ahoy"))
    println(ShowMe[Person].show(new Person("Ann", 20)))
  }
}
