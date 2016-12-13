package models

import java.io.File

import ch.ethz.dal.tinyir.util.StopWatch
import io.MyCSVReader
import postprocessing.Postprocessor
import preprocessing.{FeatureDocument, PreProcessor, Query}
import utility.Stater

import scala.collection.mutable.{ListBuffer, HashMap => MutHashMap}

// TODO: prune collection for topic model regardless of queries; prune collection
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
  val ds = DocumentSearcher(postings, collection)

  /** Language model based on topic model smoothing
    *
    * @param nRetrieval: number of retrieved documents
    * @return
    */
  def languageModel(nRetrieval: Int, intersect: Boolean = false) = {
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    // Ranking
    val scores = ListBuffer[Double]()
    val ntopics = 70
    val nVocabulary = 400000
    val nIter = 20
    var counter = 1
    val output = ListBuffer[(Int, Int, String)]()
    val mustKeptWords = preprocessedQueries.map(_.content).flatten
    val model = new LanguageModel(TokenMap, postings,
      collection.values.toSet, ntopics,
      nVocabulary, nIter,
      mustKeptWords)
    ST.start()
    for (query <- preprocessedQueries) {
      val docIndices = ds.searchDocumentsBrutely(query, intersect)//ds.searchDocumentsWithInvertedIndex(query, intersect)
      val docs = docIndices.map(collection(_))
      val retrievedDocuments = model.rankDocuments(query, docs, nRetrieval)
      if (relevJudgement.contains(query.id)) {
        val score = Postprocessor.APScore(retrievedDocuments.map(_.name), relevJudgement(query.id))
        scores += score
        println(query.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrievedDocuments.map(_.name), relevJudgement(query.id)))
      }
      else {
        output ++= Postprocessor.outputRanking(query, retrievedDocuments.map(_.name)) // output of top documents
        println(query.id)
      }
      counter += 1
    }
    ST.PrintAll()
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    (MAP, result, output.toList)
  }

  def tfidfModel(nRetrieval: Int, intersect: Boolean = false) = {
    val scores = ListBuffer[Double]()
    val vocabulary = TokenMap.map(_._2._1).toSet
    val model = new TFIDFModel(postings, collection.values.toSet)
    var counter = 1
    val output = ListBuffer[(Int, Int, String)]()
    for (query <- preprocessedQueries) {
      val docIndices = ds.searchDocumentsWithInvertedIndex(query, intersect)
      val docs = docIndices.map(collection(_))
      val retrievedDocuments = model.rankDocuments(query, docs, nRetrieval)
      if (relevJudgement.contains(query.id)) {
        val score = Postprocessor.APScore(retrievedDocuments.map(_.name), relevJudgement(query.id))
        scores += score
        println(query.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrievedDocuments.map(_.name), relevJudgement(query.id)))
      }
      else {
        output ++= Postprocessor.outputRanking(query, retrievedDocuments.map(_.name)) // output of top documents
        println(query.id)
      }
      counter += 1
    }
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    (MAP, result, output.toList)
  }

  def bm25Model(nRetrieval: Int, k: Double = 0.4, b: Double = 0.5, intersect: Boolean = false) = {
    val scores = ListBuffer[Double]()
    val vocabulary = TokenMap.map(_._2._1).toSet
    val model = new BM25(postings, collection.values.toSet, k, b)
    var counter = 1
    val output = ListBuffer[(Int, Int, String)]()
    for (query <- preprocessedQueries) {
      val docIndices = ds.searchDocumentsWithInvertedIndex(query, intersect)
      val docs = docIndices.map(collection(_))
      val retrievedDocuments = model.rankDocuments(query, docs, nRetrieval)
      if (relevJudgement.contains(query.id)) {
        val score = Postprocessor.APScore(retrievedDocuments.map(_.name), relevJudgement(query.id))
        scores += score
        println(query.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrievedDocuments.map(_.name), relevJudgement(query.id)))
      }
      else {
        output ++= Postprocessor.outputRanking(query, retrievedDocuments.map(_.name)) // output of top documents
        println(query.id)
      }
      counter += 1
    }
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    (MAP, result, output.toList)
  }

  def vectorSpaceModel(nRetrieval: Int) = {
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    val VecModel = new VectorSpaceModel(postings, collection)
    ST.PrintAll()
    println("VecModel Construted Completed!")
    val scores = ListBuffer[Double]()
    val output = ListBuffer[(Int, Int, String)]()
    for (q <- preprocessedQueries) {
      //val docs = ds.searchDocumentsWithInvertedIndex(q)
      //println("picked num: " + docs.size)
      val retrive = VecModel.rankDocuments(q)
      if (relevJudgement.contains(q.id)) {
        val score = Postprocessor.APScore(retrive, relevJudgement(q.id))
        println(q.id + ": " + "AP -> " + score + " " + "(P, R, F1) -> " + Postprocessor.f1Score(retrive, relevJudgement(q.id)))
        //println("The true num is: " + relevJudgement(q.id).size)
        scores += score
      }
      else {
        output ++= Postprocessor.outputRanking(q, retrive)
        println(q.id)
      }
      //ST.PrintAll()
    }
    val result = preprocessedQueries.map(_.id).zip(scores)
    val MAP = scores.sum / scores.length
    println(result)
    println("MAP = " + MAP)
    ST.PrintAll()
    (MAP, result, output.toList)
  }

}

object SearchEngine {
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

//    val Tuple3(score, _, output) = se.tfidfModel(100)
    val Tuple3(score, _, output) = se.bm25Model(100, 0.4, 0.5, false)
//    val Tuple3(score, _, output) = se.languageModel(100, true)
//    val Tuple3(score, _, output) = se.vectorSpaceModel(100)
//    val Tuple3(score, _, output) = se.vectorSpaceModel(100)

    ST.PrintAll()
    println(score)
    Postprocessor.writeRankingToFile("data/ranking-t-17-union-bm25.run", output) // ranking-[t|l]-[groupid].run
  }
}
