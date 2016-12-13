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

  def tf(tokens: List[Int]): HMap[Int, Int] = {
    val mm = HMap[Int, Int]()
    for (tokenID <- tokens) {
      if (!mm.contains(tokenID)) mm += tokenID -> 1
      else mm(tokenID) += 1
    }
    mm
  }

  def atf(tokens: List[String], TokenMap: HMap[String, (Int, Int)]): HMap[Int, Double] = {
    atf(tf(tokens, TokenMap))
  }

  def atf(TFs: HMap[Int, Int]): HMap[Int, Double] = {
    val mm = HMap[Int, Double]()
    val MaxValue = if (TFs.isEmpty) 1.0 else TFs.values.max.toDouble
    for (item <- TFs) {
      mm += item._1 -> (item._2.toDouble * 0.5 / MaxValue + 0.5)
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

  def atfidf(ATFs: HMap[Int, Double], postings: HMap[Int, List[Int]], Size: Int): HMap[Int, Double] = {
    val mm = HMap[Int, Double]()
    for (item <- ATFs) {
      mm += item._1 -> atfidf(ATFs(item._1), postings(item._1).length, Size)
    }
    mm
  }

  def atfidf(ATF: Double, DF: Int, Size: Int): Double = {
    log(1 + ATF) * log(Size.toDouble / DF.toDouble)
  }
}
