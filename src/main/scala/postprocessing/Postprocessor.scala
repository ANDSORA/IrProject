package postprocessing

import java.io.{BufferedWriter, File, FileWriter}

import scala.math.min
import preprocessing.Query
import math.ProbVector

/** Evaluate ranking, output results
  */
object Postprocessor {
  /** Output ranking results according to desired format (topic_id, rank, doc_id)
    *
    *
    * @param ranking
    * @return
    */
  def outputRanking(q: Query, ranking: List[String]) = {
    ranking.zipWithIndex.map{ case (doc_id, rank) => (q.id, rank + 1, doc_id)} // rank is from 1
  }

  def writeRankingToFile(dir: String, output: List[(Int, Int, String)]) = {
    val file = new File(dir)
    val bw = new BufferedWriter(new FileWriter(file))
    for (item <- output) {
      bw.write(item._1 + " " + item._2 + " " + item._3 + "\n")
    }
    bw.close()
  }

  /** Compute F1 score given P and R
    *
    * @param P
    * @param R
    * @return
    */
  def f1Score(P: Double, R: Double): Double = if (P == 0 && R == 0) 0.0 else 2 * P * R / (P + R)

  /** Compute P, R, F1
    *
    * @param retriev
    * @param relev
    * @tparam A
    */
  def f1Score[A](retriev: List[A], relev: List[A]):Tuple3[Double, Double, Double] = {
    if (retriev.isEmpty && relev.isEmpty) (1.0, 1.0, 1.0)
    else if (!relev.isEmpty && !retriev.isEmpty) {
      var TP = 0.0
      for (item <- retriev) {
        if (relev.contains(item)) TP += 1
      }
      val P = TP / retriev.length
      val R = TP / relev.length
      (P, R, f1Score(P, R))
    }
    else (0.0, 0.0, 0.0)
  }

  /** Average precision (bounded)
    *
    * @param retriev: List of document id
    * @param relev: List of ground truth document id
    * @tparam A
    * @return
    */
  def APScore[A](retriev: List[A], relev: List[A]) = {
    val retrievSize = retriev.size
    val relevanceSize = relev.size
    if (relevanceSize == 0 && retrievSize == 0) 1.0
    else if (relevanceSize > 0 && retrievSize > 0) {
      val precAt = ProbVector(new Array[Double](retrievSize)) // Precision at rank k
      val rel = ProbVector(new Array[Double](retrievSize)) // rel(k) = 1 if document at rank k is relevant; 0 otherwise
      var iter: Int = 1 // number of document so far evaluated
      var numberRelevAt: Double = 0 // number of relevant document at rank k
      for (element <- retriev) {
        if (relev.contains(element)) {
          numberRelevAt += 1
          rel(iter - 1) = 1
        }
        else rel(iter - 1) = 0
        precAt(iter - 1) = numberRelevAt / iter
        iter += 1
      }
      (precAt * rel).arr.sum / min(relevanceSize, retrievSize)
    }
    else 0.0
  }

  def main(args: Array[String]): Unit = {
    val relev = List()
    val retriev = List(0, 5, 1, 6, 7, 2, 8, 9, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5)
    println("Expected: 0.62")
    println(Postprocessor.APScore(retriev, relev))
    println(f1Score(retriev, relev))
  }
}
