package models

import java.io.File

import ch.ethz.dal.tinyir.util.StopWatch
import io.MyCSVReader
import postprocessing.Postprocessor
import preprocessing.PreProcessor._
import preprocessing.{FeatureDocument, PreProcessor, Query}
import utility.Stater

import scala.collection.mutable.{HashSet, PriorityQueue, HashMap => MutHashMap}
import scala.math._

/** BM25 model
  * k: [1.2, 2.0]; b = [0, 1]
  *
  * @param postings
  * @param collection
  */
class BM25(postings: MutHashMap[Int, List[Int]], collection: Set[FeatureDocument],
           k: Double = 0.4, b: Double = 0.5) {

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
    * @param docs
    * @param n
    * @return
    */

  def rankDocuments(q: Query, docs: Set[FeatureDocument], n: Int = 1000): List[FeatureDocument] = {
    val nRetrieval = min(n, collection.size)
    docs.map(item => (singleScore(q, item), item)).toList.sortWith(_._1 > _._1).map(_._2).take(nRetrieval)
  }
}

object BM25 {
  def main(args: Array[String]): Unit = {
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

    println("Hello, IrProject.")
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    // Create data directory
    new File("data").mkdir()
    // Load dictionary, postings, and documents

    //    val otherDir = "data/filter-1/"
    val otherDir = "data/"
    val TokenMap = PreProcessor.loadTokenMap(otherDir+ "tokenmap.txt")
    ST.PrintAll()
    val postings = PreProcessor.loadPostings(otherDir + "postings.txt")
    ST.PrintAll()
    val docs = PreProcessor.loadDocs(otherDir + "docs.txt")
    ST.PrintAll()
    // Load queries and judgement
    val relevJudgement = MyCSVReader.loadRelevJudgement("data/relevance-judgements.csv")
    val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")
    val test = MyCSVReader.loadQuery("data/test-questions.txt")
    ST.start()
    val se = SearchEngine(TokenMap, postings, docs, relevJudgement, queries ++ test)
    ST.PrintAll()

    println("Here we run the BM25 model")
    val Tuple3(score, _, output) = se.bm25Model(100, 0.4, 0.5, true)

    ST.PrintAll()
    println(score)
    Postprocessor.writeRankingToFile("data/ranking-t-17.run", output) // ranking-[t|l]-[groupid].run
  }

}
