package scala3

object ShowAsTrait:

  trait Show[T]:
    def show(x: T): String

  object Show:
    given Show[Int] with
      def show(n: Int): String = s"Int($n)"

    given Show[String] with
      def show(s: String): String = s"""String("$s")"""


  class Person(val name: String, val age: Int)

  object Person:
    given Show[Person] with
      def show(p: Person): String = s"Person(name: ${p.name}, age: ${p.age})"


  def show[T: Show](x: T): String = 
    summon[Show[T]].show(x)


  def main(args: Array[String]): Unit =
    println(show(100))
    println(show("Hello"))
    println(show(new Person("Joe", 30)))
    // println(showMe(1L))

end ShowAsTrait
