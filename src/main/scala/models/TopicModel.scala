package models

import Preprocessing.{FeatureDocument, MyDocument}
import Preprocessing.PreProcessor._
import math.ProbVector


import collection.mutable.{HashMap => MutHashMap}

/** Implement Simple Topic Model
  *
  * @param vocabulary: dictionary containing all words in collection
  * @param ntopics: number of topics
  */
class TopicModel (vocabulary: Set[Int], collection: Set[FeatureDocument], ntopics : Int) {

  /** Initialization
    * Pwt: conditional probability of a word given a topic
    * Key: each word (string)
    * Value: an array of topic probabilities P(w|t)
    *
    * Ptd: conditional probability of a topic given a document
    * Key: document id
    * Value: an array of topic distribution P(t|d)
    */

  var Pwt = MutHashMap[Int,ProbVector]()

  vocabulary.foreach(term => (Pwt += term -> ProbVector.random(ntopics).normalize))

  var Ptd = MutHashMap[FeatureDocument, ProbVector]()
  collection.foreach(doc => (Ptd += doc -> ProbVector.random((ntopics))))

  /** One iteration of the generalized Csizar algorithm to update topics of a single document
    *
    * @param Ptd
    * @param tf
    * @return
    */
  private def updateTopicSingleDocument(Ptd : ProbVector, tf: MutHashMap[Int,Int]) : ProbVector = {
    val newPtd = ProbVector(new Array[Double](ntopics))
    for ((w,f) <- tf) newPtd += (Pwt(w) * Ptd).normalize(f)
    newPtd.normalize
  }

  /** One iteration to compute updates for word distributions from single document
    *
    * @param ptd
    * @param tf
    * @return
    */
  def updateWordSingleDocument (ptd: ProbVector, tf: MutHashMap[Int,Int]) : MutHashMap[Int,ProbVector] = {
    val result = MutHashMap[Int,ProbVector]()
    for ((w,f) <- tf) result += w -> (Pwt(w) * ptd).normalize(f)
    result
  }

  /** Maximization step to update P(t|d) and P(w|t)
    *
    */
  def update = {
    val newPwt = MutHashMap[Int,ProbVector]()
    for ((doc, ptd) <- Ptd) {
      Ptd(doc) = updateTopicSingleDocument(ptd, doc.tf)
      val result = updateWordSingleDocument(Ptd(doc), doc.tf)
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
  def wordProb(w: Int, doc: FeatureDocument): Double = {
    val zeroVector = new ProbVector(new Array[Double](ntopics))
    (Pwt.getOrElse(w, zeroVector)*Ptd(doc)).arr.sum
  }
}


object TopicModel {

  def main (args : Array[String]) : Unit = {

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
    model.Pwt.foreach{ case (w,a) => println(w + ": " + a.mkString(" ")) }
    model.Ptd.foreach{ case (d, t) => println(d.ID + ": " + t.mkString(" "))}
  }
}

