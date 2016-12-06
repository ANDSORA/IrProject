package Preprocessing

import ch.ethz.dal.tinyir.processing.Document
import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.dal.tinyir.io.TipsterStream
import com.github.aztek.porterstemmer.PorterStemmer

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MutMap}

/*
import org.iq80.leveldb._
import org.iq80.leveldb.impl.Iq80DBFactory._
import java.io._
*/

/**
  * Created by andsora on 11/27/16.
  */

object PreProcessor {
  def tokenWasher(content: String): List[String] = tokenWasher(Tokenizer.tokenize(content))
  def tokenWasher(tokens: List[String]): List[String] = {
    StopWords.filterOutSW(tokens.map(a => PorterStemmer.stem(a)))
              .filter(s => s.map(c => c.isLetter).reduce(_ && _)).toList
  }

  def getTokenMap(docs: Stream[Document], TokenMap: HashMap[String, Int]): Unit = {
    var ID = 0
    var times = 0
    for (doc <- docs) {
      if ({times += 1; times} % 100 == 0) {
        println("(getTokenMap) proccessed files: " + times)
      }
      for (s <- doc.tokens) {
        if (!TokenMap.contains(s)) TokenMap += s -> {ID += 1; ID}
      }
    }
  }

  def hashMapConvertor(m: Map[String, Double], TokenMap: HashMap[String, Int]):
      HashMap[Int, Double] = {
    val hm = HashMap[Int, Double]()
    for (e <- m) {
      hm += TokenMap(e._1) -> e._2
    }
    hm
  }

  def getSample(docs: Stream[Document], TokenMap: HashMap[String, Int],
                sample: ListBuffer[(HashMap[Int, Double], Int)]): Unit = {
    var times = 0
    var ID = 0
    for (doc <- docs) {
      if ({times += 1; times} % 100 == 0) println("(getSample) processed files: " + times)
      val tf = TermFrequencies.atf(doc.tokens)
      sample += Tuple2(hashMapConvertor(tf, TokenMap), {ID += 1; ID})
    }
  }
  /*
  def UnionOfOrdered(L1: List[Int], L2: List[Int]): List[Int] = {

  }
  */
  // construct a ListBuffer with (HashMap[tfidf, vocab], DocId) This is to calculate for each document what is the tfidf weight for each token
  def gettfidfSample(docs: Stream[Document], TokenMap: HashMap[String, Int],
                sample: ListBuffer[(HashMap[Int, Double], Int)]): Unit = {
    var times = 0
    var ID = 0
    val df = MutMap[String, Int]()
    val n = docs.length
    for (doc <- docs) {
      df ++= doc.tokens.distinct.map(t => t -> (1+df.getOrElse(t,0)))
    }
    val idf = TermFrequencies.idf(df.toMap,n)
    for (doc <- docs) {
      if ({times += 1; times} % 100 == 0) println("(getSample) processed files with tfidfweight: " + times)
      val tf = TermFrequencies.tf(doc.tokens)
      val ltf = TermFrequencies.logtf(tf)
      val tfidf = ltf.map{case(k,v) => (k, v*idf(k))}
      sample += Tuple2(hashMapConvertor(tfidf, TokenMap), {ID += 1; ID})
     }

  }

  def MergeMap(Ma: Map[Int, List[Int]], Mb: Map[Int, List[Int]]): Map[Int, List[Int]] = {
    val mm = MutMap[Int, List[Int]]()
    for (item <- Ma) mm(item._1) = item._2
    for (item <- Mb) {
      if (mm.contains(item._1)) mm(item._1) = mm(item._1).union(item._2)
      else mm(item._1) = item._2
    }
    mm.toMap
  }

  def main(args: Array[String]): Unit = {
    /*
    val L1 = Map(1 -> List(10, 110), 2 -> List(20, 220), 3 -> List(30, 330))
    val L2 = Map(1 -> List(110, 1110), 4 -> List(40, 440))
    val L3 = MergeMap(L1, L2)
    println(L3)
    */
    val tips = new TipsterStream("data/raw")

    //val mm = HashMap[String, Int]()
    val ss = HashSet[String]()
    var times = 0
    for (doc <- tips.stream) {
      if ({times += 1; times} % 100 == 0) println("times: " + times)
      for (token <- tokenWasher(doc.tokens).groupBy(identity).filter(a => a._2.length >= 3).map(a => a._1)) {
        if (!ss.contains(token)) ss += token
      }
    }

    println("tokens num: " + ss.size)


    //getTokenMap(tips.stream, mm)

    //println(mm("china"))
    //println(mm("road"))
  }
}
