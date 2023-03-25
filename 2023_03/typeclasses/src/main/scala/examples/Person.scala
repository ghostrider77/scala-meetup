package examples


class Person(val name: String, val age: Int)

object Person {
  implicit val personShow: Show[Person] = new Show[Person] {
    def show(p: Person): String = s"Person(name: ${p.name}, age: ${p.age})"
  }
}
