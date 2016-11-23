/**
  * Created by andsora on 11/16/16.
  */
import breeze.linalg.DenseVector
import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.processing.StopWords

object hello extends App {
  val a = DenseVector.fill(100, 0.1)
  val b = List("are", "we", "world")
  println(a(2))
  println(StopWords.filter(b))
  println("Hello, IrProject.")
  println("Yes, and here is Li")
}
