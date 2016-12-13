package models

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
    //val docSet = DocumentSearcher(postings, collection).searchDocumentsWithInvertedIndex(q) // pure model
    val docSet = DocumentSearcher(postings, collection).searchDocumentsWithTFIDFmodel(q, 180)

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
    /*
    println("Vector Space Model.")

    println("Hello, IrProject.")
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()

    //val tips = new MyTipsterStream("data/raw")

    // Load dictionary, postings, and documents
    val TokenMap = PreProcessor.loadTokenMap("data/tokenmap.txt")
    ST.PrintAll()
    val postings = PreProcessor.loadPostings("data/postings.txt")
    ST.PrintAll()
    val docs = PreProcessor.loadDocs("data/docs.txt")
    ST.PrintAll()

    val VecModel = new VectorSpaceModel(postings, docs)
    ST.PrintAll()
    println("VecModel Construted Completed!")

    // Load queries and judgement
    val relevJudgement = MyCSVReader.loadRelevJudgement("data/relevance-judgements.csv")
    val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")
    val preprocessedQueries = queries.map(elem => new Query(elem._1,
      PreProcessor.tokenWasher(elem._2, TokenMap).map(PreProcessor.string2Id(_, TokenMap))))

    val scores = ListBuffer[Double]()
    for (q <- preprocessedQueries) {
      val retrive = VecModel.rankDocuments(q, docs.values.toSet)
      val score = Postprocessor.APScore(retrive, relevJudgement(q.id))
      println(q.id + ": " + score)
      scores += score
      ST.PrintAll()
    }

    println(preprocessedQueries.map(_.id).zip(scores))
    println("MAP = " + scores.sum / scores.length)
    ST.PrintAll()
    */

    val mm = HMap(1 -> 3.0, 2 -> 4.0)
    println(normalize(mm))
    println(abs(mm))
  }
}
