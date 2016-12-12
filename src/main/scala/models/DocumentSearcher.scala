package models

import scala.collection.mutable
import collection.mutable.{HashSet, HashMap => MutHashMap}
import scala.math.log
import preprocessing.{FeatureDocument, Query}
import preprocessing.PreProcessor._
import preprocessing.TermFeature._
import io.MyCSVReader
import ch.ethz.dal.tinyir.util.StopWatch
import utility.Stater
import scala.math.Ordering.Implicits._
import utility.ListProcesser._

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
    *
  def naiveSearchDocuments(q: Query): Set[FeatureDocument] = {
    val relatedDocuments = mutable.HashSet[FeatureDocument]()
    q.content.foreach{w =>
      if (postings.contains(w)) relatedDocuments ++= docs.filter(item => postings(w).contains(item._1)).map(_._2)
    }
    relatedDocuments.toSet
  }
  */

  def searchDocumentsWithInvertedIndex(q: Query): Set[FeatureDocument] = {
    q.content.map(postings.getOrElse(_, List())).filter(!_.isEmpty).sortBy(_.length)
      .reduceLeft((a, b) => sortedArrayUnion(a.toArray, b.toArray)).map(a => docs(a)).toSet
  }

  def searchDocumentsWithInvertedIndexNew(q: Query): Set[Int] = {
    q.content.map(postings.getOrElse(_, List())).filter(!_.isEmpty).sortBy(_.length)
      .reduceLeft((a, b) => sortedArrayUnion(a.toArray, b.toArray)).toSet
  }

  def searchDocumentsBrutely(q: Query): Set[FeatureDocument] = {
    val ss = mutable.Set[FeatureDocument]()
    for (doc <- docs.values) {
      if (q.content.map(a => doc.tf.contains(a)).reduceLeft((a,b) => a || b)) ss += doc
    }
    ss.toSet
  }

  /** Return documents based on td-idf
    *
    * @param q
    * @param d
    * @param collectionSize
    * @return
    */
  private def tfidfQueryTuple(q: Query, d: FeatureDocument, collectionSize: Int): Tuple2[Double, FeatureDocument] = {
    (q.content.map(termID => tfidf(d.tf.getOrElse(termID, 0), postings(termID).length, collectionSize)).sum, d)
  }

  private def atfidfQueryTuple(q: Query, d: FeatureDocument, collectionSize: Int): Tuple2[Double, FeatureDocument] = {
    val ATFs = atf(d.tf)
    (q.content.map(termID => atfidf(ATFs.getOrElse(termID, 0.5), postings(termID).length, collectionSize)).sum, d)
  }

  def SearchDocuments(q: Query, n: Int = 1000, IsTfIdf: Boolean = true): Set[FeatureDocument] = {
    /** Helper function to define ordering
      *
      * @param item
      * @return
      */
    def compare(item: Tuple2[Double, FeatureDocument]) = item._1

    val pq = collection.mutable.PriorityQueue[(Double, FeatureDocument)]()(Ordering.by(compare))
    for (item <- docs) {
      if (IsTfIdf == true) pq += tfidfQueryTuple(q, item._2, docs.size)
      else pq += atfidfQueryTuple(q, item._2, docs.size)
    }
    val relatedDocuments = mutable.HashSet[FeatureDocument]()
    for (i <- 1 to n) {
      relatedDocuments += pq.dequeue()._2
    }
    relatedDocuments.toSet
  }
}

object DocumentSearcher {
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

  def main(args: Array[String]): Unit = {
    val vocabulary = MutHashMap("airbus" -> (1,1),"usa" -> (2,1),
      "france" -> (3,1),"eth" -> (4,1),
      "computer" -> (5,1),"science" -> (6,1))
    val postings = MutHashMap(1 -> List(1,3), 2 -> List(1), 3 -> List(1, 3),
      4 -> List(2, 3), 5 -> List(2), 6 -> List(2, 3))
    val ntopics = 2
    val doc1       = new FeatureDocument(1, "doc_1", tf(tokenWasher("usa france airbus", false), vocabulary))
    val doc2       = new FeatureDocument(2, "doc_2", tf(tokenWasher("eth computer science", false), vocabulary))
    val doc3       = new FeatureDocument(3, "doc_3", tf(tokenWasher("airbus eth france science", false),vocabulary))
    val collection = MutHashMap(1 -> doc1, 2 -> doc2, 3 -> doc3)
    val ds = DocumentSearcher(postings, collection)
    val query = Query(0, List(1,2))
    println(ds.searchDocumentsWithInvertedIndex(query))
  }
}
