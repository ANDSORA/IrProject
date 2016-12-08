import Preprocessing.{FeatureDocument, PreProcessor, Query}
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
    val ntopics = 40
    val nIter = 50
    val vocabulary = TokenMap.map(_._2._1).toSet
    var counter = 1
    for (query <- preprocessedQueries) {
      val relatedDocuments = DocumentSearcher(postings, collection).tfidfSearchDocuments(query, nRetrieval * 2).toSet
      val model = new TopicModel(vocabulary, relatedDocuments, ntopics)
      model.learn(nIter)
      val ranker = new PointwiseRanker(query, nRetrieval)
      val ranking = ranker.rankDocs(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, model.wordProb, 0.1))(relatedDocuments)
      scores += Postprocessor.APScore(ranking.map(_._1), relevJudgement(query.id))
      println(counter + "\n")
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
    val scores = ListBuffer[Double]()
    val vocabulary = TokenMap.map(_._2._1).toSet
    val model = new TFIDFModel(postings, collection.values.toSet)
    var counter = 1
    for (query <- preprocessedQueries) {
      val retrievedDocuments = model.rankDocuments(query, nRetrieval)
      scores += Postprocessor.APScore(retrievedDocuments.map(_.name).toList, relevJudgement(query.id))
      println(counter + "\n")
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

  def bm25Model(nRetrieval: Int) = {
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    val scores = ListBuffer[Double]()
    val vocabulary = TokenMap.map(_._2._1).toSet
    val model = new BM25(postings, collection.values.toSet)
    var counter = 1
    for (query <- preprocessedQueries) {
      val retrievedDocuments = model.rankDocuments(query, nRetrieval)
      scores += Postprocessor.APScore(retrievedDocuments.map(_.name), relevJudgement(query.id))
      println(counter + "\n")
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
}
object SearchEngine {
  def main(args: Array[String]): Unit = {
    println("Hello, IrProject.")
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    val tips = new MyTipsterStream("data/raw")
    // Load dictionary, postings, and documents
    val TokenMap = PreProcessor.loadTokenMap("data/tokenmap.txt")
    ST.PrintAll()
    val postings = PreProcessor.loadPostings("data/postings.txt")
    ST.PrintAll()
    val docs = PreProcessor.loadDocs("data/docs.txt")
    ST.PrintAll()
    // Load queries and judgement
    val relevJudgement = MyCSVReader.loadRelevJudgement("data/relevance-judgements.csv")
    val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")

    val se = SearchEngine(TokenMap, postings, docs, relevJudgement, queries)
    se.bm25Model(100)
//    se.tfidfModel(100)
  }
}
