package models

import ch.ethz.dal.tinyir.util.StopWatch
import io.{MyCSVReader, MyTipsterStream}
import postprocessing.Postprocessor
import preprocessing.{FeatureDocument, PreProcessor, Query}
import preprocessing.TermFeature._
import utility.{Stater, WordProber}

import scala.collection.mutable.{HashMap => HMap}
import scala.math.sqrt
import scala.collection.mutable.ListBuffer

/**
  * Created by andsora on 12/8/16.
  */
class VectorSpaceModel(val postings: HMap[Int, List[Int]], val docs: HMap[Int, FeatureDocument]) {
  def echo_self(): Unit = println("VectorSpaceModel.")

  //val DocVectors = HMap[Int, HMap[Int, Double]]() // HMap[docID, HMap[termID, tfidf]]
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

  def query(q: Query, nRetrieval: Int = 100): List[String] = {
    // construct the query vector, simple way
    val qVec = HMap[Int, Double]()
    for (termID <- q.content) {
     if (!qVec.contains(termID)) qVec += termID -> 1.0
    }

    // get the doc set to compute with
    val docSet = DocumentSearcher(postings, docs).SearchDocuments(q, 1000).map(doc => doc.ID).toList

    // return the best 100
    docSet.map(docID => (docs(docID).name, cos(docID, qVec))).sortBy(- _._2).take(nRetrieval).map(_._1)
  }

  private def cos(docID: Int, vec: HMap[Int, Double]): Double = cos(tfidf(docs(docID).tf, postings, docs.size), vec)
  //private def cos(docID: Int, vec: HMap[Int, Double]): Double = cos(atf(docs(docID).tf), vec)

  private def cos(vec1: HMap[Int, Double], vec2: HMap[Int, Double]): Double = {
    dot(vec1, vec2) / (abs(vec1) * abs(vec2))
  }

  private def dot(vec1: HMap[Int, Double], vec2: HMap[Int, Double]): Double = {
    vec1.map(item => vec2.getOrElse(item._1, 0.0) * item._2).sum
  }

  private def abs(vec: HMap[Int, Double]): Double = {
    sqrt(vec.values.map(v => v*v).sum)
  }
}

object VectorSpaceModel {
  def main(args: Array[String]): Unit = {
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
      val retrive = VecModel.query(q)
      val score = Postprocessor.APScore(retrive, relevJudgement(q.id))
      println(q.id + ": " + score)
      scores += score
      ST.PrintAll()
    }

    println(preprocessedQueries.map(_.id).zip(scores))
    println("MAP = " + scores.sum / scores.length)
    ST.PrintAll()

    /*
    // Ranking
    val scores = ListBuffer[Double]()
    val ntopics = 40
    val nInter = 100
    val vocabulary = TokenMap.map(_._2._1).toSet
    var counter = 1
    for (query <- preprocessedQueries) {
      val collection = DocumentSearcher(postings, docs).tfidfSearchDocuments(query, 1000)
      val model = new TopicModel(vocabulary, collection, ntopics)
      model.learn(nInter)
      val ranker = new PointwiseRanker(query)
      val ranking = ranker.rankDocs(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, model.wordProb, 0.1))(collection)
      //    val ranking = ranker.rankDocs(WordProber.dirichletSmoothedWordProb(model.wordProb, 1))(collection)
      scores += Postprocessor.APScore(ranking.map(_._1), relevJudgement(query.id))
      println(counter + "\n")
      counter += 1
      ST.PrintAll()
    }
    println(preprocessedQueries.map(_.id).zip(scores))
    println("MAP = " + scores.sum / scores.length)
    ST.PrintAll()
    */
  }
}
