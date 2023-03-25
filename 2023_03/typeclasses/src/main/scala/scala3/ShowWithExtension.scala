package scala3

object ShowWithExtension:

  trait Show[T]:
    extension(x: T)
      def show: String

  object Show:
    given Show[Int] with
      extension(n: Int)
        def show: String = s"Int($n)"

    given Show[String] with
      extension(s: String)
        def show: String = s"""String("$s")"""


  class Person(val name: String, val age: Int)

  object Person:
    given Show[Person] with
      extension(p: Person)
        def show: String = s"Person(name: ${p.name}, age: ${p.age})"


  def show[T: Show](x: T): String = x.show

  def main(args: Array[String]): Unit =
    println(show(100))
    println(show("Hello"))
    println(show(new Person("Joe", 30)))
  // println(showMe(1L))

end ShowWithExtension
