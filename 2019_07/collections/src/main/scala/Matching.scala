
object Matching {
  import scala.annotation.tailrec
  import scala.collection.breakOut

  private def readInput(filename: String): Iterator[String] = scala.io.Source.fromResource(filename).getLines()

  private def calcFirstOccurrencePositions(string: Vector[Char]): Map[Char, Int] = {
    val letterCounts: Map[Char, Int] = string.groupBy(identity).mapValues(_.length)
    letterCounts
      .keysIterator
      .toList
      .sorted
      .foldLeft((Map.empty[Char, Int], 0)) {
        case ((acc, ix), letter) => (acc + (letter -> ix), ix + letterCounts(letter))
      }._1
  }

  private def calcCountMatrix1(transformedString: Vector[Char], uniqueLetters: Set[Char]): Map[Char, Vector[Int]] = {
    val length: Int = transformedString.length
    def count(string: Vector[Char], letter: Char): Vector[Int] = {
      @tailrec
      def loop(acc: List[Int], ix: Int): Vector[Int] = {
        if (ix == length) acc.reverse.toVector
        else {
          val char: Char = string(ix)
          val lastCount: Int = acc.head
          val nextCount: Int = if (char == letter) lastCount + 1 else lastCount
          loop(nextCount :: acc, ix + 1)
        }
      }
      loop(List(0), 0)
    }

    (for { letter <- uniqueLetters } yield letter -> count(transformedString, letter))(breakOut)
  }

  private def calcCountMatrix2(transformedString: Vector[Char], uniqueLetters: Set[Char]): Map[Char, Vector[Int]] = {
    val countMatrix: Map[Char, Array[Int]] =
      uniqueLetters.map(letter => letter -> Array.fill(transformedString.length + 1)(0))(breakOut)
    for {
      (letter, ix) <- transformedString.view.zipWithIndex
      char <- uniqueLetters
    } {
      val counts: Array[Int] = countMatrix(char)
      counts(ix + 1) = if (letter == char) counts(ix) + 1 else counts(ix)
    }

    countMatrix.mapValues(_.toVector)
  }

  private def letterOccursBetweenPointers(letter: Char, lastColumn: Vector[Char], top: Int, bottom: Int): Boolean =
    lastColumn.slice(top, bottom + 1).contains(letter)

  private def patternMatching(pattern: String,
                              lastColumn: Vector[Char],
                              firstOccurrences: Map[Char, Int],
                              countMatrix: Map[Char, Vector[Int]]): Int = {
    @tailrec
    def loop(reversedPattern: List[Char], top: Int, bottom: Int): Int = reversedPattern match {
      case Nil => bottom - top + 1
      case letter :: rest =>
        if (!letterOccursBetweenPointers(letter, lastColumn, top, bottom)) 0
        else {
          val letterOccurrence: Int = firstOccurrences(letter)
          val letterCounter: Vector[Int] = countMatrix(letter)
          loop(rest, letterOccurrence + letterCounter(top), letterOccurrence + letterCounter(bottom + 1) - 1)
        }
    }

    loop(pattern.reverseIterator.toList, 0, lastColumn.length - 1)
  }

  def improvedBWPatternMatching(transformedString: Vector[Char], patterns: List[String]): List[Int] = {
    val firstOccurrences: Map[Char, Int] = calcFirstOccurrencePositions(transformedString)
    val uniqueLetters: Set[Char] = firstOccurrences.keySet
    val countMatrix1: Map[Char, Vector[Int]] = calcCountMatrix1(transformedString, uniqueLetters)
    val countMatrix2: Map[Char, Vector[Int]] = calcCountMatrix2(transformedString, uniqueLetters)
    println(countMatrix1 == countMatrix2)
    patterns.map(patternMatching(_, transformedString, firstOccurrences, countMatrix2))
  }

  def main(args: Array[String]): Unit = {
    //val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val t0 = System.nanoTime()
    val reader: Iterator[String] = readInput("input08.txt")
    val transformedString: Vector[Char] = reader.next().toVector
    val _: Int = reader.next().toInt
    val patterns: List[String] = reader.next().split(" ").toList
    println(s"Length of text: ${transformedString.length}")
    println(s"Number of patterns: ${patterns.length}")
    val nrMatches: List[Int] = improvedBWPatternMatching(transformedString, patterns)
    val t1 = System.nanoTime()
    println((t1 - t0) / 1e9)
  }
}
