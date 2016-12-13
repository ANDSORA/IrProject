import ch.ethz.dal.tinyir.util.StopWatch
import io.{MyCSVReader, MyTipsterStream}
import utility.{Stater, WordProber}

import preprocessing.Query
import preprocessing.PreProcessor._
import postprocessing.Postprocessor

import scala.collection.mutable.ListBuffer


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

object Starter extends App {
  println("Hello, IrProject.")
  val ST = new Stater(new StopWatch, Runtime.getRuntime)
  ST.start()

  val tips = new MyTipsterStream("data/raw")

  val T1 = tips.stream.toIterator
  val TokenMap = getTokenMap(T1, 1)

  val T2 = tips.stream.toIterator
  val Tuple2(postings, docs) = getPostingsAndDocs(T2, TokenMap, ST)

  val relevJudgement = MyCSVReader.loadRelevJudgement("data/relevance-judgements.csv")
  val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")
  val test = MyCSVReader.loadQuery("data/test-questions.txt")

  //val se = SearchEngine(TokenMap, postings, docs, relevJudgement, queries)
  ST.PrintAll()
  println("All preparation work are done.")

  //    val Tuple3(score, _, output) = se.tfidfModel(100)
  //    val Tuple3(score, _, output) = se.bm25Model(100, 0.4, 0.5)
  //    val Tuple3(score, _, output) = se.vectorSpaceModel(100)
  //val Tuple3(score, _, output) = se.vectorSpaceModel(100)
  //ST.PrintAll()
  //println(score)
  //Postprocessor.writeRankingToFile("data/ranking-l-17.run", output) // ranking-[t|l]-[groupid].run
}
