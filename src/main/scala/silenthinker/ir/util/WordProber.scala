package silenthinker.ir.util

/**
  * Created by Junlin on 11/30/16.
  * Provide various methods to compute word probability given a document, i.e., P(w|d)
  */
object WordProber {
  /**
    * Naively compute word probability through term frequency
    */
  def naiveWordProb(w: String, doc: Map[String, Int]): Double = {
    doc.getOrElse(w, 0).toDouble / doc.values.sum
  }

  /**
    * Compute word probability P(w|d)
    * where P(w|d) = (1-lambda)*P^^(w|d) + lambda*P^^top(w|d)
    * Note that lambda is invariant for all documents
    */
  // TODO: lambda depends on documents
  def jmSmoothedWordProb(firstWordProb: (String, Map[String, Int]) => Double,
                         secondWordProb: (String, Map[String, Int]) => Double,
                         lambda: Double)(w: String, doc: Map[String, Int]) = {
    assert(lambda <= 1 && lambda >= 0, "Smoothing parameter should be between 0 and 1.")
    (1 - lambda) * firstWordProb(w, doc) + lambda * secondWordProb(w, doc)
  }
}
