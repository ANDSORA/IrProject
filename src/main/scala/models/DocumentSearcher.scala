package models

import scala.collection.mutable
import collection.mutable.{HashSet, HashMap => MutHashMap}
import scala.math.log
import Preprocessing.{FeatureDocument, PreProcessor, Query}
import Preprocessing.PreProcessor._
import io.MyCSVReader
import ch.ethz.dal.tinyir.util.StopWatch
import utility.Stater
import scala.math.Ordering.Implicits._


/**
  * Created by Junlin on 12/7/16.
  */

/** Search documents based on query and postings
  *
  * @param postings
  */
case class DocumentSearcher(val postings: MutHashMap[Int, List[Int]], val docs: MutHashMap[Int, FeatureDocument]) {

  /** Return related documents which contains at least one word of query
    *
    * @param q
    */
  def naiveSearchDocuments(q: Query): Set[FeatureDocument] ={
    val relatedDocuments = mutable.HashSet[FeatureDocument]()
    q.content.foreach{w =>
      if (postings.contains(w)) relatedDocuments ++= docs.filter(item => postings(w).contains(item._1)).map(_._2)
    }
    relatedDocuments.toSet
  }

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
    * @param n: number of returned documents
    * @return
    */
  def tfidfSearchDocuments(q: Query, n: Int = 1000): Set[FeatureDocument] = {
    /** Helper function to define ordering
      *
      * @param item
      * @return
      */
    def compare(item: Tuple2[Double, FeatureDocument]) = item._1

    val pq = collection.mutable.PriorityQueue[(Double, FeatureDocument)]()(Ordering.by(compare))
    for (item <- docs) {
      pq += tfidf(q, item._2, docs.size)
    }
    val relatedDocuments = mutable.HashSet[FeatureDocument]()
    for (i <- 1 to n) {
      relatedDocuments += pq.dequeue()._2
    }
    relatedDocuments.toSet
  }
}

object DocumentSearcher extends App {
  /**
  val ST = new Stater(new StopWatch, Runtime.getRuntime)
  ST.start()
  val postings = loadPostings("data/postings.txt")
  ST.PrintAll()
  val TokenMap = PreProcessor.loadTokenMap("data/tokenmap.txt")
  ST.PrintAll()
  val docs = PreProcessor.loadDocs("data/docs.txt")
  val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")
  ST.PrintAll()
  val preprocessedQueries = queries.map(elem => new Query(elem._1,
    PreProcessor.tokenWasher(elem._2, TokenMap).map(PreProcessor.string2Id(_, TokenMap))))
  println(preprocessedQueries.head)
  println(DocumentSearcher(postings, docs).naiveSearchDocuments(preprocessedQueries.head))
    */
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
  val ds = DocumentSearcher(postings, collection)
  val query = Query(0, List(1,2))
  println(ds.tfidfSearchDocuments(query, 3))

}
