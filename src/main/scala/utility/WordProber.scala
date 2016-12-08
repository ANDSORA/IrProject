package utility

import ch.ethz.dal.tinyir.lectures.TermFrequencies.tf
import preprocessing.{FeatureDocument, MyDocument}
import collection.mutable.{HashMap => MutHashMap}
import com.sun.org.apache.xalan.internal.utils.FeatureManager.Feature

/**
  * Created by Junlin on 11/30/16.
  * Provide various methods to compute word probability given a document, i.e., P(w|d)
  */
object WordProber {
  /**
    * Naively compute word probability through term frequency
    * @param w
    * @param doc
    */
  def naiveWordProb(w: Int, doc: FeatureDocument): Double = {
    doc.tf.getOrElse(w, 0).toDouble / doc.tf.values.sum.toDouble
  }

  /** Implement Bayesian smoothing using Dirichlet priors
    *
    * @param collectionProb
    * @param mu
    * @param w
    * @param doc
    */
  def dirichletSmoothedWordProb(collectionProb: (Int, FeatureDocument) => Double,
                                mu: Double = 3000)(w: Int, doc: FeatureDocument): Double = {
    (doc.tf.getOrElse(w, 0) + mu * collectionProb(w, doc)) / (doc.tf.values.sum + mu)
  }

/**
  * Compute word probability P(w|d)
  * where P(w|d) = (1-lambda)*P^^(w|d) + lambda*P^^top(w|d)
  * Note that lambda is invariant for all documents
  * @param firstWordProb
  * @param collectionProb
  * @param lambda
  */
  def jmSmoothedWordProb(firstWordProb: (Int, FeatureDocument) => Double,
                         collectionProb: (Int, FeatureDocument) => Double,
                         lambda: Double = 0.1)(w: Int, doc: FeatureDocument): Double = {
    assert(lambda <= 1 && lambda >= 0, "Smoothing parameter should be between 0 and 1.")
    (1 - lambda) * firstWordProb(w, doc) + lambda * collectionProb(w, doc)
  }
}
