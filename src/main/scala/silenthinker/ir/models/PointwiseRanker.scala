package silenthinker.ir.models

import silenthinker.ir.util.{StopWatch, WordProber}

/**
  * Created by Junlin on 11/30/16.
  *
  * Class that implements pointwise ranking algorithm
  */

case class PointwiseRanker(val q: List[String]) {
  /**
    * Compute the query probability: P(q|d) = product of P(w|d) where w belongs to query q
   */
  def queryScore(wordProb: (String, Map[String, Int]) => Double)(doc: Map[String, Int]):Double = {
    q.map(wordProb(_, doc)).product
  }

  /**
    * Rank documents according to query likelihood
    */
  def rankDocs(wordProb: (String, Map[String, Int]) => Double, docs: Stream[Map[String, Int]]) = {
    docs.map(queryScore(wordProb)(_)).zipWithIndex.sortBy(_._1)
  }
}

object PointwiseRanker extends App {
  val vocabulary = Set("a1","a2","a3","b1","b2","b3")
  val ntopics    = 2
  val model      = new TopicModel(vocabulary,ntopics)
  val doc1       = Map("a1" -> 1, "a2" ->1, "a3" ->1)
  val doc2       = Map("b1" -> 1, "b2" ->1, "b3" ->1)
  val doc3       = Map("a1" -> 1, "b2" ->1, "a3" ->1, "b1" ->1)
  val stream     = Stream(doc1, doc2, doc3)

  model.learn(stream)
  val query = List("a1", "a3", "a2")
  val ranker = new PointwiseRanker(query)
  println(ranker.rankDocs(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, model.wordProb, 0.2), stream).toList)

}
