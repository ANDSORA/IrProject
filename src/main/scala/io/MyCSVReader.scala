package io

import scala.collection.mutable
import scala.collection.mutable.{MutableList, Set => MutSet}
import scala.io.Source
import Preprocessing.PreProcessor
import ch.ethz.dal.tinyir.processing.Tokenizer.tokenize

/** Read relevance judgement .csv file
  *
  */
class MyCSVReader {

  /** Load and read relevance judgement csv file
    *
    * @param dir
    * @return Map[Int, Set[String\]\]: map each topic to relevant documents
    */
  def loadRelevJudgement(dir: String): Map[Int, Set[String]] = {
    val relevJudgement = MutSet[(Int, String)]()
    val bufferedSource = Source.fromFile(dir)
    bufferedSource.getLines().foreach { line =>
      val Array(topic, ignore, doc_id, relevant) = line.split(" ").map(_.trim())
      if (relevant == "1") relevJudgement += Tuple2(topic.toInt, doc_id)
    }
    relevJudgement.groupBy(_._1).mapValues(elem => elem.map(_._2).toSet)
  }

  /** Load queries file
    *
    * @param dir
    * @return query id mapped to unpreprocessed query content
    */
  def loadQuery(dir: String): List[(Int, String)] = {
    val bufferedSource = Source.fromFile(dir)
    val queries = mutable.MutableList[String]()
    val query_id = mutable.MutableList[Int]()
    val sep_1 = "Number:"
    val sep_2 = "Topic:"
    bufferedSource.getLines().foreach{ line =>
      if (line.contains(sep_1)) {
        val Array(tag, id) = line.split(sep_1).map(_.trim())
        query_id += id.toInt
      }
      if (line.contains(sep_2)) {
        val Array(tag, query) = line.split(sep_2).map((_.trim()))
        queries += query
      }
    }
    query_id.zip(queries).toList
  }
}

object MyCSVReader extends App {
  val dir = "data/relevance-judgements.csv"
  val tokenmap = PreProcessor.loadTokenMap("data/tokenmap.txt")
  val csvReader = new MyCSVReader
  val relevJudgement = csvReader.loadRelevJudgement(dir)
//  println(relevJudgement.get(51).mkString("\n"))
  val queries = csvReader.loadQuery("data/questions-descriptions.txt")
  val preprocessedQueries = queries.map(elem => PreProcessor.tokenWasher(elem._2, tokenmap))
  println(queries.map(_._2).zip(preprocessedQueries).mkString("\n"))
}

