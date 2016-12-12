import preprocessing.{FeatureDocument, PreProcessor, Query}
import ch.ethz.dal.tinyir.util.StopWatch
import io.{MyCSVReader, MyTipsterStream}
import models._
import postprocessing.Postprocessor
import utility.{Stater, WordProber}

import scala.collection.mutable.{ListBuffer, HashMap => MutHashMap}

/** Encapsulate all models to rank and evaluate
  *
  */
case class SearchEngine(TokenMap: MutHashMap[String, (Int, Int)],
                   postings: MutHashMap[Int, List[Int]],
                   collection: MutHashMap[Int, FeatureDocument],
                   relevJudgement: Map[Int, List[String]],
                   queries: List[(Int, String)]) {

  // Preprocess queries
  val preprocessedQueries = queries.map(elem => new Query(elem._1,
    PreProcessor.tokenWasher(elem._2, TokenMap).map(PreProcessor.string2Id(_, TokenMap))))

  /** Language model based on topic model smoothing
    *
    * @param nRetrieval: number of retrieved documents
    * @return
    */
  def languageModel(nRetrieval: Int) = {
    // Ranking
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    val scores = ListBuffer[Double]()
    val ntopics = 60
    val nVocabulary = 500000
    val nIter = 10
    var counter = 1
    val mustKeptWords = preprocessedQueries.map(_.content).flatten
    val model = new LanguageModel(TokenMap, postings,
      collection.values.toSet, ntopics,
      nVocabulary, nIter,
      mustKeptWords)
    for (query <- preprocessedQueries) {
      val retrievedDocuments = model.rankDocuments(query, nRetrieval)
      val score = Postprocessor.APScore(retrievedDocuments.map(_.name), relevJudgement(query.id))
      scores += score
      println(query.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrievedDocuments.map(_.name), relevJudgement(query.id)))
      counter += 1
      ST.PrintAll()
    }
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    ST.PrintAll()
    (MAP, result)
  }

  def tfidfModel(nRetrieval: Int) = {
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    val scores = ListBuffer[Double]()
    val vocabulary = TokenMap.map(_._2._1).toSet
    val model = new TFIDFModel(postings, collection.values.toSet)
    var counter = 1
    for (query <- preprocessedQueries) {
      val retrievedDocuments = model.rankDocuments(query, nRetrieval)
      val score = Postprocessor.APScore(retrievedDocuments.map(_.name), relevJudgement(query.id))
      scores += score
      println(query.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrievedDocuments.map(_.name), relevJudgement(query.id)))
      counter += 1
    }
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    ST.PrintAll()
    (MAP, result)
  }

  def bm25Model(nRetrieval: Int, k: Double = 1.6, b: Double = 0.75) = {
    val scores = ListBuffer[Double]()
    val vocabulary = TokenMap.map(_._2._1).toSet
    val model = new BM25(postings, collection.values.toSet, k, b)
    var counter = 1
    for (query <- preprocessedQueries) {
      val retrievedDocuments = model.rankDocuments(query, nRetrieval)
      val score = Postprocessor.APScore(retrievedDocuments.map(_.name), relevJudgement(query.id))
      scores += score
      println(query.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrievedDocuments.map(_.name), relevJudgement(query.id)))
      counter += 1
    }
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    (MAP, result)
  }

  def vectorSpaceModel(nRetrieval: Int) = {
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    val VecModel = new VectorSpaceModel(postings, collection)
    ST.PrintAll()
    println("VecModel Construted Completed!")

    val scores = ListBuffer[Double]()
    for (q <- preprocessedQueries) {
      val retrive = VecModel.query(q)
      val score = Postprocessor.APScore(retrive, relevJudgement(q.id))
      println(q.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrive, relevJudgement(q.id)))
      println("The true num is: " + relevJudgement(q.id).size + "\n")
      scores += score
      //ST.PrintAll()
    }
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    ST.PrintAll()
    (MAP, result)
  }

}

object SearchEngine {
  def main(args: Array[String]): Unit = {
    println("Hello, IrProject.")
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    // Load dictionary, postings, and documents
    val otherDir = "data/filter-10/"
    val TokenMap = PreProcessor.loadTokenMap(otherDir+ "tokenmap.txt")
    ST.PrintAll()
    val postings = PreProcessor.loadPostings(otherDir + "postings.txt")
    ST.PrintAll()
    val docs = PreProcessor.loadDocs(otherDir + "docs.txt")
    ST.PrintAll()
    // Load queries and judgement
    val relevJudgement = MyCSVReader.loadRelevJudgement("data/relevance-judgements.csv")
    val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")

    val se = SearchEngine(TokenMap, postings, docs, relevJudgement, queries)
    var score = ListBuffer[Double]()

//    score += se.tfidfModel(100)._1
//    score += se.languageModel(100)._1
//    score += se.bm25Model(100, 0.4, 0.5)._1
    score += se.vectorSpaceModel(100)._1

    println(score)
  }
}
