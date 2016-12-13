package models

import java.io.File

import ch.ethz.dal.tinyir.util.StopWatch
import io.MyCSVReader
import postprocessing.Postprocessor
import preprocessing.{FeatureDocument, PreProcessor, Query}
import utility.{Stater, WordProber}

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
    /*
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
    */
    println("Hello, IrProject.")
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    // Create data directory
    new File("data").mkdir()
    // Load dictionary, postings, and documents

    //    val otherDir = "data/filter-1/"
    val otherDir = "data/"
    val TokenMap = PreProcessor.loadTokenMap(otherDir+ "tokenmap.txt")
    ST.PrintAll()
    val postings = PreProcessor.loadPostings(otherDir + "postings.txt")
    ST.PrintAll()
    val docs = PreProcessor.loadDocs(otherDir + "docs.txt")
    ST.PrintAll()
    // Load queries and judgement
    val relevJudgement = MyCSVReader.loadRelevJudgement("data/relevance-judgements.csv")
    val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")
    val test = MyCSVReader.loadQuery("data/test-questions.txt")
    ST.start()
    val se = SearchEngine(TokenMap, postings, docs, relevJudgement, queries ++ test)
    ST.PrintAll()

    println("Here we run the Language model.")
    val Tuple3(score, _, output) = se.languageModel(100)

    ST.PrintAll()
    println(score)
    Postprocessor.writeRankingToFile("data/ranking-l-17.run", output) // ranking-[t|l]-[groupid].run
  }
}
