package models

import Preprocessing.PreProcessor._
import Preprocessing.{FeatureDocument, MyDocument, Query}
import postprocessing.Postprocessor
import utility.WordProber
import collection.mutable.{HashMap => MutHashMap}

/**
  * Created by Junlin on 11/30/16.
  *
  * Class that implements pointwise ranking algorithm
  */

case class PointwiseRanker(val q: Query) {
  /**
    * Compute the query probability: P(q|d) = product of P(w|d) where w belongs to query q
   */
  def queryScore(wordProb: (Int, FeatureDocument) => Double)(doc: FeatureDocument):Double = {
    q.content.map(wordProb(_, doc)).product
  }

  /** Rank documents according to query likelihood
    *
    * @param wordProb
    * @param docs
    * @return
    */
  def rankDocs(wordProb: (Int, FeatureDocument) => Double)(docs: Set[FeatureDocument]): List[(String, Double)] = {
    docs.map(doc => (doc.name, queryScore(wordProb)(doc))).toList.sortWith(_._2 > _._2)
  }

}

object PointwiseRanker extends App {
  val vocabulary = MutHashMap("airbus" -> (1,1),"usa" -> (2,1),
    "france" -> (3,1),"eth" -> (4,1),
    "computer" -> (5,1),"science" -> (6,1))
  val ntopics = 2
  val doc0       = new FeatureDocument(0, "doc_0", tf(tokenWasher("usa france airbus"), vocabulary))
  val doc1       = new FeatureDocument(1, "doc_1", tf(tokenWasher("eth computer science"), vocabulary))
  val doc2       = new FeatureDocument(2, "doc_2", tf(tokenWasher("airbus eth france science"),vocabulary))
  val collection = Set(doc0, doc1, doc2)
  val model      = new TopicModel(vocabulary.map(_._2._1).toSet, collection, ntopics)


  var count = 0.0
  model.learn(100)

  val query = Query(0, List(1,2))
  val ranker = new PointwiseRanker(query)
  val ranking = ranker.rankDocs(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, model.wordProb, 0.1))(collection).toList
  println(Postprocessor.outputRanking(query, ranking))

}
