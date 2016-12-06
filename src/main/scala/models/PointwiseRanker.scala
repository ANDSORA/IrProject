package models

import Preprocessing.{MyDocument, Query}
import postprocessing.Postprocessor
import utility.WordProber

/**
  * Created by Junlin on 11/30/16.
  *
  * Class that implements pointwise ranking algorithm
  */

case class PointwiseRanker(val q: Query) {
  /**
    * Compute the query probability: P(q|d) = product of P(w|d) where w belongs to query q
   */
  def queryScore(wordProb: (String, MyDocument) => Double)(doc: MyDocument):Double = {
    q.content.map(wordProb(_, doc)).product
  }

  /** Rank documents according to query likelihood
    *
    * @param wordProb
    * @param docs
    * @return
    */
  def rankDocs(wordProb: (String, MyDocument) => Double)(docs: Stream[MyDocument]) = {
    docs.map(doc => (doc.name, queryScore(wordProb)(doc))).sortWith(_._2 > _._2)
  }

}

object PointwiseRanker extends App {

  val vocabulary = Set("airbus","usa","france","eth","computer","science")
  val ntopics = 2
  val doc0       = new MyDocument(0, "doc_0", "usa france airbus")
  val doc1       = new MyDocument(1, "doc_1", "eth computer science")
  val doc2       = new MyDocument(2, "doc_2", "airbus eth france science")
  val doc3       = new MyDocument(3, "doc_3", "usa computer airbus airbus france usa france airbus")
  val stream     = Stream(doc0, doc1, doc2, doc3)
  val model      = new TopicModel(vocabulary, stream, ntopics)

  model.learn(50)
  val query = Query(0, List("airbus", "usa"))
  val ranker = new PointwiseRanker(query)
  val ranking = ranker.rankDocs(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, model.wordProb, 0.1))(stream).toList
  println(Postprocessor.outputRanking(query, ranking))
}
