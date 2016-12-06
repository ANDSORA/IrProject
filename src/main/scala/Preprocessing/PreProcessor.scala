package Preprocessing

import ch.ethz.dal.tinyir.processing.Document
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.dal.tinyir.io.TipsterStream
import com.github.aztek.porterstemmer.PorterStemmer

import scala.collection.mutable.HashSet
import scala.collection.mutable.{HashMap => HMap}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MutMap}

import utility.Stater
import utility.StopWatch
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


  /** Method to filter out stop words, non-alphabetical words from
    * already tokenized words
    *
    * @param tokens
    * @return
    */
  def tokenWasher(tokens: List[String]): List[String] = {
    StopWords.filterOutSW(tokens)
              .filter(s => s.map(c => c.isLetter).reduce(_ && _)).toList
  }

  def getTokenMap(it: Iterator[Document], thres: Int): HMap[String, (Int, Int)] = {
    var termID = 0
    var times = 0
    val mm = HMap[String, Int]()
    val MM = HMap[String, (Int, Int)]()
    for (doc <- it) {
      if ({times += 1; times} % 100 == 0) {
        println("(getTokenMap) proccessed files: " + times)
      }
      for (s <- tokenWasher(doc.tokens)) {
        if (!mm.contains(s)) mm += s -> 1
        else mm(s) += 1
      }
    }
    for (item <- mm.filter(a => a._2 >= thres)) {
      MM += item._1 -> ({termID += 1; termID}, item._2)
    }
    MM
  }

  def getPostings(it: Iterator[Document], TokenMap: HMap[String, (Int, Int)], ST: Stater):
    HMap[Int, ListBuffer[Int]] = {
    var docID = 0
    var times = 0
    val mm = HMap[Int, ListBuffer[Int]]()
    for (doc <- it) {
      if ({times += 1; times} % 100 == 0) {
        println("(getPostings) proccessed files: " + times)
        if (times % 1000 == 0) {
          println("The size: " + mm.size)
          ST.PrintAll()
        }
      }
      docID += 1
      for (token <- doc.tokens) {
        if (TokenMap.contains(token)) {
          val termID = TokenMap(token)._1
          if (!mm.contains(termID)) mm += termID -> ListBuffer(docID)
          else if (mm(termID).last != docID) mm(termID) += docID
        }
      }
    }
    mm
  }

  /*
  def hashMapConvertor(m: Map[String, Double], TokenMap: HMap[String, Int]):
      HMap[Int, Double] = {
    val hm = HMap[Int, Double]()
    for (e <- m) {
      hm += TokenMap(e._1) -> e._2
    }
    hm
  }

  def getSample(it: Iterator[Document], TokenMap: HMap[String, (Int, Int)],
                sample: ListBuffer[(HMap[Int, Double], Int)]): Unit = {
    var times = 0
    var ID = 0
    for (doc <- it) {
      if ({times += 1; times} % 100 == 0) println("(getSample) processed files: " + times)
      val tf = TermFrequencies.atf(doc.tokens)
      sample += Tuple2(hashMapConvertor(tf, TokenMap), {ID += 1; ID})
    }
  }
  */

  /*
  def UnionOfOrdered(L1: List[Int], L2: List[Int]): List[Int] = {

  }
  */

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
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()

    val tips = new TipsterStream("data/raw")
    val It_1 = tips.stream.toIterator
    val TokenMap = getTokenMap(It_1, 10)
    println("The size of Map = " + TokenMap.size)
    ST.PrintAll()
    //println(TokenMap.filter(aa => aa._2._2 == 15))

    val It_2 = tips.stream.toIterator
    val postings = getPostings(It_2, TokenMap, ST)
    println(postings.take(100))
    ST.PrintAll()
  }
}
