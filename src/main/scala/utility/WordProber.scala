package utility

import ch.ethz.dal.tinyir.lectures.TermFrequencies.tf
import Preprocessing.MyDocument

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
  def naiveWordProb(w: String, doc: MyDocument): Double = {
    val termFreq = tf(doc.tokens)
    termFreq.getOrElse(w, 0).toDouble / termFreq.values.sum.toDouble
  }

  // TODO: lambda depends on documents

/**
  * Compute word probability P(w|d)
  * where P(w|d) = (1-lambda)*P^^(w|d) + lambda*P^^top(w|d)
  * Note that lambda is invariant for all documents
  * @param firstWordProb
  * @param secondWordProb
  * @param lambda
  */
  def jmSmoothedWordProb(firstWordProb: (String, MyDocument) => Double,
                         secondWordProb: (String, MyDocument) => Double,
                         lambda: Double)(w: String, doc: MyDocument) = {
    assert(lambda <= 1 && lambda >= 0, "Smoothing parameter should be between 0 and 1.")
    (1 - lambda) * firstWordProb(w, doc) + lambda * secondWordProb(w, doc)
  }
}
