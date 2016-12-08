import scala.collection.mutable.PriorityQueue
import scala.math
//  import scala.collection.mutable.PriorityQueue

def diff(t2: (Int,Int)) = math.abs(t2._1 - t2._2)
// diff: (t2: (Int, Int))Int

val x = new PriorityQueue[(Int, Int)]()(Ordering.by(diff))
// x: scala.collection.mutable.PriorityQueue[(Int, Int)] = PriorityQueue()

x.enqueue(1 -> 1)
x.enqueue(1 -> 2)
x.enqueue(1 -> 3)
x.enqueue(1 -> 4)
x.enqueue(1 -> 0)
x.dequeue()
x.dequeue()
x.dequeue()
x.dequeue()

1 to 3