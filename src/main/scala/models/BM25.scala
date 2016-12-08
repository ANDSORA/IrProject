package models

import preprocessing.PreProcessor._
import preprocessing.{FeatureDocument, Query}

import scala.collection.mutable.{HashMap => MutHashMap, PriorityQueue, HashSet}
import scala.math._

/** BM25 model
  * k: [1.2, 2.0]; b = 0.75
  *
  * @param postings
  * @param collection
  */
class BM25(postings: MutHashMap[Int, List[Int]], collection: Set[FeatureDocument],
           k: Double = 1.6, b: Double = 0.75) {

  val collectionSize = collection.size
  /** Average document length
    *
    */
  val avgdl = {
    collection.map(d => d.tf.values.sum).sum.toDouble / collectionSize
  }

  /** Compute tf
    *
    * @param w
    * @return
    */
  def tf(w: Int, d: FeatureDocument) = d.tf.getOrElse(w, 0)

  /** Compute idf
    *
    * @param w
    * @return
    */
  def idf(w: Int) = log(collectionSize / postings(w).length.toDouble)

  def singleScore(q: Query, d: FeatureDocument): Double = {
    var score = 0.0
    for (w <- q.content) {
      score += idf(w) * tf(w, d) * (k + 1) / (tf(w, d) + k * (1 - b + b * d.tf.values.sum / avgdl))
    }
    score
  }

  /** Rank documents
    *
    * @param q
    * @param n
    * @return
    */

  def rankDocuments(q: Query, n: Int = 1000) = {
    val nRetrieval = min(n, collection.size)
    collection.map(item => (singleScore(q, item), item)).toList.sortWith(_._1 > _._1).map(_._2).take(nRetrieval)
  }
}

object BM25 extends App {
  /*
  val vocabulary = MutHashMap("airbus" -> (1,1),"usa" -> (2,1),
    "france" -> (3,1),"eth" -> (4,1),
    "computer" -> (5,1),"science" -> (6,1))
  val postings = MutHashMap(1 -> List(1,3), 2 -> List(1), 3 -> List(1, 3),
    4 -> List(2, 3), 5 -> List(2), 6 -> List(2, 3))
  val ntopics = 2
  val doc1       = new FeatureDocument(1, "doc_1", tf(tokenWasher("usa france airbus"), vocabulary))
  val doc2       = new FeatureDocument(2, "doc_2", tf(tokenWasher("eth computer science"), vocabulary))
  val doc3       = new FeatureDocument(3, "doc_3", tf(tokenWasher("airbus eth france science"),vocabulary))
  val collection = MutHashMap(1 -> doc1, 2 -> doc2, 3 -> doc3)
  val bm25 = new BM25(postings, collection.values.toSet)
  val query = Query(0, List(1,2))
  println(bm25.rankDocuments(query))
  */
}
