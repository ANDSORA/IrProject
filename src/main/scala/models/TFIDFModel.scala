package models

import java.io.File

import ch.ethz.dal.tinyir.util.StopWatch
import io.MyCSVReader
import postprocessing.Postprocessor
import preprocessing.{FeatureDocument, PreProcessor, Query}
import utility.Stater

import scala.collection.mutable.{HashSet, PriorityQueue, HashMap => MutHashMap}
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

object TFIDFModel {
  def main(args: Array[String]): Unit = {
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

    println("Here we run the TFIDF model.")
    val Tuple3(score, _, output) = se.tfidfModel(100)

    ST.PrintAll()
    println(score)
    Postprocessor.writeRankingToFile("data/ranking-t-17-tfidf.run", output) // ranking-[t|l]-[groupid].run
  }
}