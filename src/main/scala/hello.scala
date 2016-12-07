import ch.ethz.dal.tinyir.util.StopWatch
import io.{MyCSVReader, MyTipsterStream}
import utility.{Stater, WordProber}
import Preprocessing.{PreProcessor, Query}
import models.{DocumentSearcher, PointwiseRanker, TopicModel}
import postprocessing.Postprocessor

import scala.collection.mutable.ListBuffer

/**
  * Created by andsora on 11/16/16.
  */

/*
class MyThread extends Runnable {
  def run: Unit = {
    /* memory info */
    val runtime = Runtime.getRuntime
    while (true) {
      Thread.sleep(1000)
      utility.Stater.PrintMeM(runtime)
    }
  }
}
*/

object hello extends App {
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
  val preprocessedQueries = queries.map(elem => new Query(elem._1,
    PreProcessor.tokenWasher(elem._2, TokenMap).map(PreProcessor.string2Id(_, TokenMap))))

  // Ranking
  val scores = ListBuffer[Double]()
  val ntopics = 50
  val nInter = 50
  val vocabulary = TokenMap.map(_._2._1).toSet
  for (query <- preprocessedQueries) {
    val collection = DocumentSearcher(postings, docs).findRelatedDocuments(query)
    val model = new TopicModel(vocabulary, collection, ntopics)
    model.learn(nInter)
    val ranker = new PointwiseRanker(query)
    val ranking = ranker.rankDocs(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, model.wordProb, 0.1))(collection).toList
    scores += Postprocessor.APScore(ranking.map(_._1), relevJudgement(query.id))
    ST.PrintAll()
  }
  println(preprocessedQueries.map(_.id).zip(scores))
  println("MAP = " + scores.sum / scores.length)
  ST.PrintAll()
}
