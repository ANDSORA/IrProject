package preprocessing

import scala.collection.mutable.{HashMap => HMap}
import scala.math.log

/**
  * Created by andsora on 12/8/16.
  */
object TermFeature {
  def tf(tokens: List[String], TokenMap: HMap[String, (Int, Int)]): HMap[Int, Int] = {
    val mm = HMap[Int, Int]()
    for (token <- tokens) {
      val termID = PreProcessor.string2Id(token, TokenMap)
      if (!mm.contains(termID)) mm += termID -> 1
      else mm(termID) += 1
    }
    mm
  }

  def tfidf(TFs: HMap[Int, Int], postings: HMap[Int, List[Int]], Size: Int): HMap[Int, Double] = {
    val mm = HMap[Int, Double]()
    for (item <- TFs) {
      mm += item._1 -> tfidf(TFs(item._1), postings(item._1).length, Size)
    }
    mm
  }

  def tfidf(TF: Int, DF: Int, Size: Int): Double = {
    log(1 + TF.toDouble) * log(Size.toDouble / DF.toDouble)
  }
}
