package models

import Preprocessing.MyDocument
import Preprocessing.PreProcessor
import math.ProbVector
import ch.ethz.dal.tinyir.lectures.TermFrequencies.tf
import collection.mutable.{HashMap => MutHashMap}

/** Implement Simple Topic Model
  *
  * @param vocabulary: dictionary containing all words in collection
  * @param ntopics: number of topics
  */
class TopicModel (vocabulary: Set[String], collection: Stream[MyDocument], ntopics : Int) {

  /** Initialization
    * Pwt: conditional probability of a word given a topic
    * Key: each word (string)
    * Value: an array of topic probabilities P(w|t)
    *
    * Ptd: conditional probability of a topic given a document
    * Key: document id
    * Value: an array of topic distribution P(t|d)
    */
  var Pwt = MutHashMap[String,ProbVector]()
  vocabulary.foreach(term => (Pwt += term -> ProbVector.random(ntopics).normalize))

  var Ptd = MutHashMap[MyDocument, ProbVector]()
  collection.foreach(doc => (Ptd += doc -> ProbVector.random((ntopics))))

  /** One iteration of the generalized Csizar algorithm to update topics of a single document
    *
    * @param Ptd
    * @param doc
    * @return
    */
  private def updateTopicSingleDocument(Ptd : ProbVector, doc: Map[String,Int]) : ProbVector = {
    val newPtd = ProbVector(new Array[Double](ntopics))
    for ((w,f) <- doc) newPtd += (Pwt(w) * Ptd).normalize(f)
    newPtd.normalize
  }

  /** One iteration to compute updates for word distributions from single document
    */
  def updateWordSingleDocument (ptd: ProbVector, doc: Map[String,Int]) : MutHashMap[String,ProbVector] = {
    val result = MutHashMap[String,ProbVector]()
    for ((w,f) <- doc) result += w -> (Pwt(w) * ptd).normalize(f)
    result
  }

  /** Maximization step to update P(t|d) and P(w|t)
    *
    */
  def update = {
    val newPwt = MutHashMap[String,ProbVector]()
    for ((doc, ptd) <- Ptd) {
      val termFreq = tf(doc.tokens)
      Ptd(doc) = updateTopicSingleDocument(ptd, termFreq)
      val result = updateWordSingleDocument(Ptd(doc), termFreq)
      val increment = result.map{
        case (k,v) => k -> ( if (newPwt.contains(k)) v + newPwt(k) else v)
      }
      increment.foreach{ case (k,a) => newPwt(k) = a }
    }
    Pwt = newPwt
    val sums = Pwt.values.reduce((v1,v2) => v1 + v2)
    Pwt.foreach{ case (s,a) => Pwt.update(s,a/sums) } // normalization
  }

  /** Iteratively update P(t|d) and P(w|t)
    *
    *
    * @param n_iter
    */
  def learn(n_iter: Int = 20) = for (i <- 0 until n_iter) update


  /**
    * Compute P(w|d) = \sum_t=1->T P(w|t)*P(t|d)
    */
  def wordProb(w: String, doc: MyDocument): Double = {
    val zeroVector = new ProbVector(new Array[Double](ntopics))
    (Pwt.getOrElse(w, zeroVector)*Ptd(doc)).arr.sum
  }
}


object TopicModel {

  def main (args : Array[String]) : Unit = {

    val vocabulary = Set("airbus","usa","france","eth","computer","science")
    val ntopics = 2
    val doc0       = new MyDocument(0, "doc_0", "usa france airbus")
    val doc1       = new MyDocument(1, "doc_1", "eth computer science")
    val doc2       = new MyDocument(2, "doc_2", "airbus eth france science")
    val stream     = Stream(doc0, doc1, doc2)
    val model      = new TopicModel(vocabulary, stream, ntopics)


    var count = 0.0
    model.learn(100)
    model.Pwt.foreach{ case (w,a) => println(w + ": " + a.mkString(" ")) }
    model.Ptd.foreach{ case (d, t) => println(d.ID + ": " + t.mkString(" "))}
  }
}

