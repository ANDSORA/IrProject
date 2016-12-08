import ch.ethz.dal.tinyir.util.StopWatch
import io.{MyCSVReader, MyTipsterStream}
import utility.{Stater, WordProber}
import Preprocessing.{PreProcessor, Query}
import models.{DocumentSearcher, PointwiseRanker, TopicModel}
import postprocessing.Postprocessor

import scala.collection.mutable.ListBuffer

/** ntopics nIter n_related_doc MAP
  *   40     100     1000
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

  /** Uncomment to first create dictionary, postings, and preprocessed documents
    *
  val It_1 = tips.stream.toIterator
  val TokenMap = PreProcessor.getTokenMap(It_1, 10)
  println("The size of Map = " + TokenMap.size)
  ST.PrintAll()

  val It_2 = tips.stream.toIterator
  val result = PreProcessor.getPostingsAndDocs(It_2, TokenMap, ST)
  val postings = result._1
  val docs = result._2
  PreProcessor.saveDocs("data/docs.txt", docs)
  PreProcessor.saveTokenMap("data/tokenmap.txt", TokenMap)
  PreProcessor.savePostings("data/postings.txt", postings)
  ST.PrintAll()
    */

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
  val ntopics = 40
  val nIter = 50
  val vocabulary = TokenMap.map(_._2._1).toSet
  var counter = 1
  for (query <- preprocessedQueries) {
    val collection = DocumentSearcher(postings, docs).tfidfSearchDocuments(query, 100)
    /*
    val model = new TopicModel(vocabulary, collection, ntopics)
    model.learn(nIter)
    val ranker = new PointwiseRanker(query)
    val ranking = ranker.rankDocs(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, model.wordProb, 0.1))(collection)
//    val ranking = ranker.rankDocs(WordProber.dirichletSmoothedWordProb(model.wordProb, 1))(collection)

    scores += Postprocessor.APScore(ranking.map(_._1), relevJudgement(query.id))
    */
    scores += Postprocessor.APScore(collection.map(_.name).toList, relevJudgement(query.id))
    println(counter + "\n")
    counter += 1
    ST.PrintAll()
  }
  println(preprocessedQueries.map(_.id).zip(scores))
  println("MAP = " + scores.sum / scores.length)
  ST.PrintAll()
}
