package models

import preprocessing.{FeatureDocument, PreProcessor, Query}
import utility.WordProber
import scala.collection.mutable.{HashMap => MutHashMap}
import scala.math._
import preprocessing.TermFeature.tf

/**
  * Created by Junlin on 12/10/16.
  */
class LanguageModel(TokenMap: MutHashMap[String, (Int, Int)],
                    postings: MutHashMap[Int, List[Int]],
                    collection: Set[FeatureDocument],
                    ntopics: Int,
                    nVocabulary: Int = 10000,
                    nIter: Int = 20,
                    mustKeptWords: List[Int] = List()
                   ) {
  // Prune vocabulary
  val vocabulary = PreProcessor.vocabularyPruner(TokenMap, postings, collection, nVocabulary, mustKeptWords)
  val topicModel = new TopicModel (vocabulary, collection, ntopics)
  topicModel.learn(nIter)

  /** Compute single document score
    *
    * @param q
    * @param d
    * @return
    */
  def singleScore(q: Query, d: FeatureDocument): Double = {
    q.content.map(WordProber.jmSmoothedWordProb(WordProber.naiveWordProb, topicModel.wordProb)(_, d)).product
  }

  /** Rank documents
    *
    * @param q
    * @param docs
    * @param n
    * @return
    */
  def rankDocuments(q: Query, docs: Set[FeatureDocument], n: Int = 100): List[FeatureDocument] = {
    val nRetrieval = min(n, collection.size)
    docs.map(item => (singleScore(q, item), item)).toList.sortWith(_._1 > _._1).map(_._2).take(nRetrieval)
  }
}

object LanguageModel {
  def main(args: Array[String]): Unit = {
    val vocabulary = MutHashMap("airbus" -> (1,1),"usa" -> (2,1),
      "france" -> (3,1),"eth" -> (4,1),
      "computer" -> (5,1),"science" -> (6,1))
    val postings = MutHashMap(1 -> List(1,3), 2 -> List(1), 3 -> List(1, 3),
      4 -> List(2, 3), 5 -> List(2), 6 -> List(2, 3))
    val ntopics = 2
    val doc1       = new FeatureDocument(1, "doc_1", tf(PreProcessor.tokenWasher("usa france airbus", false), vocabulary))
    val doc2       = new FeatureDocument(2, "doc_2", tf(PreProcessor.tokenWasher("eth computer science", false), vocabulary))
    val doc3       = new FeatureDocument(3, "doc_3", tf(PreProcessor.tokenWasher("airbus eth france science", false),vocabulary))
    val collection = MutHashMap(1 -> doc1, 2 -> doc2, 3 -> doc3)
    val model = new LanguageModel(vocabulary, postings, collection.values.toSet, 2, 1, 20)
    val query = Query(0, List(1,2))
    println(model.rankDocuments(query, collection.values.toSet))
  }
}
