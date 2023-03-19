import org.scalameter._

import scala.annotation.tailrec

object Config {
  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer new Warmer.Default
}

object CountEvens {
  def countEvens1(lst: List[Int]): Int = lst.count(_ % 2 == 0)

  def countEvens2(lst: List[Int]): Int = {
    var count: Int = 0
    for {
      elem <- lst
      if elem % 2 == 0
    } count += 1
    count
  }

  def countEvens3(lst: List[Int]): Int =
    lst.foldLeft(0)((acc, elem) => if (elem % 2 == 0) acc + 1 else acc)

  def countEvens3b(lst: List[Int]): Int =
    lst.foldRight(0)((elem, acc) => if (elem % 2 == 0) acc + 1 else acc)

  def countEvens4(lst: List[Int]): Int = {
    @tailrec
    def loop(xs :List[Int], acc: Int): Int = xs match {
      case Nil => acc
      case x :: xss => if (x % 2 == 0) loop(xss, acc + 1) else loop(xss, acc)
    }
    loop(lst, 0)
  }

  def countEvens5(lst: List[Int]): Int = {
    @tailrec
    def loop(xs :List[Int], acc: Int): Int =
      if (xs.isEmpty) acc
      else if (xs.head % 2 == 0) loop(xs.tail, acc + 1)
      else loop(xs.tail, acc)

    loop(lst, 0)
  }
}

object Experiment {
  import util.Random._
  import Config.standardConfig
  import CountEvens._

  def generateData(size: Int): List[Int] = {
    setSeed(2112)
    (0 until size).map(_ => nextInt()).toList
  }

  def main(args: Array[String]): Unit = {
    val size: Int = 1000000
    val list: List[Int] = generateData(size)
    val time: Quantity[Double] = standardConfig.measure {
      countEvens3b(list)
    }
    println(time)
  }
}
