package models

import preprocessing.{FeatureDocument, Query}
import scala.collection.mutable.{HashMap => MutHashMap, HashSet, PriorityQueue}
import scala.math._

/** Term-based model using tf-idf
  *
  * @param postings
  * @param collection
  */
class TFIDFModel(postings: MutHashMap[Int, List[Int]], collection: Set[FeatureDocument]) {

  /** Compute tf-idf
    *
    * @param q
    * @param d
    * @param collectionSize
    * @return
    */
  def tfidf(q: Query, d: FeatureDocument, collectionSize: Int): Tuple2[Double, FeatureDocument] = {
    def singletfidf(w: Int): Double = {
      log(1 + d.tf.getOrElse(w, 0).toDouble) * log(collectionSize / postings(w).length.toDouble)
    }
    (q.content.map(singletfidf(_)).sum, d)
  }


  /** Return documents based on td-idf
    *
    * @param q
    * @param docs
    * @param n: number of returned documents
    * @return
    */
  def rankDocuments(q: Query, docs: Set[FeatureDocument], n: Int = 1000): List[FeatureDocument] = {
    val nRetrieval = min(n, collection.size)
    docs.map(item => tfidf(q, item, collection.size)).toList.sortWith(_._1 > _._1).map(_._2).take(nRetrieval)
  }
}
