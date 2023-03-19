import scala.annotation.tailrec

object MedianOfStream {
  import scala.annotation.tailrec
  import scala.collection.mutable.{PriorityQueue => Heap}

  private def calcMedian(maxHeap: Heap[Int], minHeap: Heap[Int]): Int = {
    val s1: Int = maxHeap.size
    val s2: Int = minHeap.size
    if (s1 > s2) maxHeap.head
    else if (s2 > s1) minHeap.head
    else (maxHeap.head + minHeap.head) / 2
  }

  private def addToHeap(n: Int, currentMedian: Int, maxHeap: Heap[Int], minHeap: Heap[Int]): Unit = {
    if (n < currentMedian) maxHeap.enqueue(n)
    else minHeap.enqueue(n)
  }

  private def balanceHeaps(maxHeap: Heap[Int], minHeap: Heap[Int]): Unit = {
    val s1: Int = maxHeap.size
    val s2: Int = minHeap.size
    if (s1 > s2 + 1) {
      val largestLeft: Int = maxHeap.dequeue()
      minHeap.enqueue(largestLeft)
    } else if (s2 > s1 + 1) {
      val smallestRight: Int = minHeap.dequeue()
      maxHeap.enqueue(smallestRight)
    }
  }

  def calcRunningMedians(numbers: List[Int]): List[Int] = {
    val maxHeap = Heap.empty[Int]
    val minHeap = Heap.empty[Int](Ordering[Int].reverse)

    @tailrec
    def loop(acc: List[Int], currentMedian: Int, ns: List[Int]): List[Int] = ns match {
      case Nil => acc.reverse
      case n :: nss =>
        addToHeap(n, currentMedian, maxHeap, minHeap)
        balanceHeaps(maxHeap, minHeap)
        val m: Int = calcMedian(maxHeap, minHeap)
        loop(m :: acc, m, nss)
    }

    numbers match {
      case Nil => Nil
      case first :: rest =>
        minHeap.enqueue(first)
        loop(List(first), first, rest)
    }
  }
}

object ImmutableTree {
  import scala.collection.immutable.TreeSet

  def calcRunningMedians(numbers: List[Int]): List[Int] = {
    @tailrec
    def loop(treeSet: TreeSet[(Int, Int)], medians: List[Int], ns: List[(Int, Int)]): List[Int] = ns match {
      case Nil => medians.reverse
      case (n, ix) :: nss =>
        val updatedSet: TreeSet[(Int, Int)] = treeSet.insert((n, ix))
        val size: Int = updatedSet.size
        val m: Int =
          if (size % 2 == 1) {
            val List(elem) = updatedSet.slice(size / 2, size / 2 + 1).toList.map(_._1)
            elem
          }
          else {
            val List(a, b) = updatedSet.slice(size / 2 - 1, size / 2 + 1).toList.map(_._1)
            (a + b) / 2
          }
        loop(updatedSet, m :: medians, nss)
    }

    loop(TreeSet.empty[(Int, Int)], Nil, numbers.zipWithIndex)
  }

}

object MutableTree {
  import scala.collection.mutable.{TreeSet => RBTree}

  def calcRunningMedians(numbers: List[Int]): List[Int] = {
    val tree = RBTree.empty[(Int, Int)]
    @tailrec
    def loop(medians: List[Int], ns: List[(Int, Int)]): List[Int] = ns match {
      case Nil => medians.reverse
      case (n, ix) :: nss =>
        tree += ((n, ix))
        val size: Int = tree.size
        val m: Int =
          if (size % 2 == 1) tree.slice(size / 2, size / 2 + 1).head._1
          else {
            val List(a, b) = tree.slice(size / 2 - 1, size / 2 + 1).toList.map(_._1)
            (a + b) / 2
          }
        loop(m :: medians, nss)
    }

    loop(Nil, numbers.zipWithIndex)
  }
}

object Naive {
  def calcRunningMedians(numbers: List[Int]): List[Int] = {
    @tailrec
    def loop(medians: List[Int], prefix: Vector[Int], ns: List[Int]): List[Int] = ns match {
      case Nil => medians.reverse
      case n :: nss =>
        val updated: Vector[Int] = n +: prefix
        val size: Int = updated.size
        val sorted: Vector[Int] = updated.sorted
        val m: Int =
          if (size % 2 == 1) sorted(size / 2)
          else (sorted(size / 2 - 1) + sorted(size / 2)) / 2
        loop(m :: medians, sorted, nss)
    }
    loop(Nil, Vector(), numbers)
  }
}

object Experiment2 {
  import scala.util.Random

  def generateNumbers(size: Int): List[Int] = {
    val r = new Random(42)
    (0 until size).map(_ => r.nextInt()).toList
  }

  def main(args: Array[String]): Unit = {
    //val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    //val nrLines: Int = reader.next().toInt
    //val numbers: List[Int] = reader.take(nrLines).map(_.toInt).toList
    //val numbers = List(12, 8, 1, 1, 1, 1, 1, 2, 3)
    val numbers = generateNumbers(100000)
    val t0 = System.nanoTime()
    val result: List[Int] = Naive.calcRunningMedians(numbers)
    val t1 = System.nanoTime()
    println((t1 - t0) / 1e9)
    //result.foreach(println)
  }
}
