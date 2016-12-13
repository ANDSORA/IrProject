package models

import java.io.File

import ch.ethz.dal.tinyir.util.StopWatch
import io.{MyCSVReader, MyTipsterStream}
import postprocessing.Postprocessor
import preprocessing.{FeatureDocument, PreProcessor, Query}
import preprocessing.TermFeature._
import utility.ListProcesser._
import utility.{Stater, WordProber}

import scala.collection.mutable.{HashMap => HMap}
import scala.math.sqrt
import scala.collection.mutable.ListBuffer

/**
  * Created by andsora on 12/8/16.
  */
class VectorSpaceModel(val postings: HMap[Int, List[Int]], val collection: HMap[Int, FeatureDocument]) {
  def echo_self(): Unit = println("VectorSpaceModel.")

  private val DocVectors = HMap[Int, HMap[Int, Double]]() // HMap[docID, HMap[termID, tfidf]]
  /*
  val DocVectors: HMap[Int, HMap[Int, Double]] = { // HMap[docID, HMap[termID, tfidf]]
    val mm = HMap[Int, HMap[Int, Double]]()
    val Size = docs.size
    for (item <- docs) {
      mm += item._1 -> tfidf(item._2.tf, postings, Size)
    }
    mm
  }
  */

  /** Rank documents
    *
    * @param q
    * @param nRetrieval
    * @return
    */
  def rankDocuments(q: Query, nRetrieval: Int = 100): List[String] = {
    // construct the query vector, simple way
    val qVec = normalize(tfidf(tf(q.content), postings, collection.size))

    // get the docSet
    val docSet = DocumentSearcher(postings, collection).searchDocumentsWithInvertedIndex(q, true) // pure model
    //val docSet = DocumentSearcher(postings, collection).searchDocumentsWithTFIDFmodel(q, 180)

    // do the job of vector space model
    docSet.map(d => (collection(d).name, cos(d, qVec))).toList.sortBy(- _._2).map(_._1).take(nRetrieval)
    //docSet.map(docID => collection(docID).name).toList
  }

  private def normalize(vec: HMap[Int, Double]): HMap[Int, Double] = {
    val ABS = abs(vec)
    for (key <- vec.keys) vec(key) /= ABS
    vec
  }

  //private def cos(docID: Int, vec: HMap[Int, Double]): Double = cos(tfidf(docs(docID).tf, postings, docs.size), vec)
  private def cos(docID: Int, vec: HMap[Int, Double]): Double = {
    if (!DocVectors.contains(docID)) DocVectors += docID -> normalize(tfidf(collection(docID).tf, postings, collection.size))
    cos(DocVectors(docID), vec)
  }

  private def cos(d: FeatureDocument, vec: HMap[Int, Double]): Double = cos(tfidf(d.tf, postings, collection.size), vec)
  //private def cos(docID: Int, vec: HMap[Int, Double]): Double = cos(atf(docs(docID).tf), vec)

  private def cos(vec1: HMap[Int, Double], vec2: HMap[Int, Double]): Double = {
    dot(vec1, vec2)
  }

  private def dot(vec1: HMap[Int, Double], vec2: HMap[Int, Double]): Double = {
    vec1.map(item => vec2.getOrElse(item._1, 0.0) * item._2).sum
  }

  private def abs(vec: HMap[Int, Double]): Double = {
    sqrt(vec.values.map(v => v*v).sum)
  }
}

object VectorSpaceModel {
  def normalize(vec: HMap[Int, Double]): HMap[Int, Double] = {
    val ABS = abs(vec)
    for (key <- vec.keys) vec(key) /= ABS
    vec
  }

  def abs(vec: HMap[Int, Double]): Double = {
    sqrt(vec.values.map(v => v*v).sum)
  }

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

    println("Here we run the Vector Space Model.")
    val Tuple3(score, _, output) = se.vectorSpaceModel(100)

    ST.PrintAll()
    println(score)
    Postprocessor.writeRankingToFile("data/ranking-t-17-vem.run", output) // ranking-[t|l]-[groupid].run
  }
}
