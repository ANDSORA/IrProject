package Preprocessing

import ch.ethz.dal.tinyir.processing.Document
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.util.StopWatch
import scala.collection.mutable.{HashMap => HMap, ListBuffer, Map => MutMap}

import utility.Stater
import io.MyTipsterStream


/**
  * Created by andsora on 11/27/16.
  */


object PreProcessor {
  def tokenWasher(content: String): List[String] = tokenWasher(Tokenizer.tokenize(content))
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
      for (s <- tokenWasher(doc.tokens) ++ tokenWasher(doc.title)) {
        if (!mm.contains(s)) mm += s -> 1
        else mm(s) += 1
      }
    }
    for (item <- mm.filter(a => a._2 >= thres)) {
      MM += item._1 -> ({termID += 1; termID}, item._2)
    }
    MM
  }

  def getPostingsAndDocs(it: Iterator[Document], TokenMap: HMap[String, (Int, Int)],
                         postings: HMap[Int, ListBuffer[Int]], docs: HMap[Int, Document], ST: Stater): Unit = {
    var docID = 0
    var times = 0
    for (doc <- it) {
      // print the memory usage and time
      if ({times += 1; times} % 100 == 0) {
        println("(getPostings) proccessed files: " + times)
        if (times % 1000 == 0) {
          println("The size: " + postings.size)
          ST.PrintAll()
        }
      }

      // docID ++
      docID += 1

      // fill docs
      val prunedTokens = doc.tokens.filter(token => TokenMap.contains(token))
      docs += docID -> new FeatureDocument(doc.ID, doc.name, tf(prunedTokens, TokenMap), doc.title)

      // fill postings
      for (token <- prunedTokens ++ tokenWasher(doc.title).filter(token => TokenMap.contains(token))) {
        val termID = TokenMap(token)._1
        if (!postings.contains(termID)) postings += termID -> ListBuffer(docID)
        else if (postings(termID).last != docID) postings(termID) += docID
      }
    }
  }

  def getDocs(it: Iterator[Document], docs: HMap[Int, Document]): Unit = {
    var docID = 0
    var times = 0
    for (doc <- it) {
      if ({times += 1; times} % 100 == 0) {
        println("(getDocs) proccessed files: " + times)
      }
      docs += {docID += 1; docID} -> doc
    }
  }

  def tf(tokens: List[String], TokenMap: HMap[String, (Int, Int)]): HMap[Int, Int] = {
    val mm = HMap[Int, Int]()
    for (token <- tokens) {
      val termID = TokenMap(token)._1
      if (!mm.contains(termID)) mm += termID -> 1
      else mm(termID) += 1
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

  def MergeMap(Ma: Map[Int, List[Int]], Mb: Map[Int, List[Int]]): Map[Int, List[Int]] = {
    val mm = MutMap[Int, List[Int]]()
    for (item <- Ma) mm(item._1) = item._2
    for (item <- Mb) {
      if (mm.contains(item._1)) mm(item._1) = mm(item._1).union(item._2)
      else mm(item._1) = item._2
    }
    mm.toMap
  }
  */

  def main(args: Array[String]): Unit = {
    /*
    val L1 = Map(1 -> List(10, 110), 2 -> List(20, 220), 3 -> List(30, 330))
    val L2 = Map(1 -> List(110, 1110), 4 -> List(40, 440))
    val L3 = MergeMap(L1, L2)
    println(L3)
    */
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()

    val tips = new MyTipsterStream("data/raw")

    val It_1 = tips.stream.toIterator
    val TokenMap = getTokenMap(It_1, 10)
    println("The size of Map = " + TokenMap.size)
    ST.PrintAll()
    //println(TokenMap.filter(aa => aa._2._2 == 15))

    val It_2 = tips.stream.toIterator
    val postings = HMap[Int, ListBuffer[Int]]()
    val docs = HMap[Int, Document]()
    getPostingsAndDocs(It_2, TokenMap, postings, docs, ST)
    //getDocs(It_2, docs)
    println(postings.take(100))
    println(docs.take(10))
    ST.PrintAll()
  }

}
