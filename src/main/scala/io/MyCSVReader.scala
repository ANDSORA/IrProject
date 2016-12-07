package io

import scala.collection.mutable.{Set => MutSet}
import scala.io.Source

/** Read relevance judgement .csv file
  *
  */
class MyCSVReader {

  /** Load and read csv file
    *
    * @param dir
    * @return Map[Int, Set[String\]\]: map each topic to relevant documents
    */
  def load(dir: String): Map[Int, Set[String]] = {
    val relevJudgement = MutSet[(Int, String)]()
    val bufferedSource = Source.fromFile(dir)
    bufferedSource.getLines().foreach { line =>
      val Array(topic, ignore, doc_id, relevant) = line.split(" ").map(_.trim())
      if (relevant == "1") relevJudgement += Tuple2(topic.toInt, doc_id)
    }
    relevJudgement.groupBy(_._1).mapValues(elem => elem.map(_._2).toSet)
  }
}

object MyCSVReader extends App {
  val dir = "data/relevance-judgements.csv"
  val csvReader = new MyCSVReader
  val relevJudgement = csvReader.load(dir)
  println(relevJudgement.get(51).mkString("\n"))
}
