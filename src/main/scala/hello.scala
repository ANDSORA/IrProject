import ch.ethz.dal.tinyir.util.StopWatch
import io.{MyCSVReader, MyTipsterStream}
import utility.{Stater, WordProber}

import preprocessing.Query
import preprocessing.PreProcessor._
import models.{DocumentSearcher, PointwiseRanker, TopicModel, VectorSpaceModel}
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

  ///** Uncomment to first create dictionary, postings, and preprocessed documents
/*
  val It_1 = tips.stream.toIterator
  val TokenMap = PreProcessor.getTokenMap(It_1, 10)
  println("The size of Map = " + TokenMap.size)
  ST.PrintAll()

  val It_2 = tips.stream.toIterator
  val result = PreProcessor.getPostingsAndDocs(It_2, TokenMap, ST)
  val postings = result._1
  val docs = result._2
  saveDocs("data/docs.txt", docs)
  saveTokenMap("data/tokenmap.txt", TokenMap)
  savePostings("data/postings.txt", postings)
*/


  // Load dictionary, postings, and documents
  val TokenMap = loadTokenMap("data/tokenmap.txt")
  ST.PrintAll()
  val postings = loadPostings("data/postings.txt")
  ST.PrintAll()
  val docs = loadDocs("data/docs.txt")
  ST.PrintAll()
  // Load queries and judgement
  val relevJudgement = MyCSVReader.loadRelevJudgement("data/relevance-judgements.csv")
  val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")
  val preprocessedQueries = queries.map(elem => new Query(elem._1,
    tokenWasher(elem._2, TokenMap).map(string2Id(_, TokenMap))))

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
