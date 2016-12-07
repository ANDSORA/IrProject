package Preprocessing

import java.io.{BufferedWriter, File, FileWriter}

import ch.ethz.dal.tinyir.processing.Document
//import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.util.StopWatch

import scala.collection.mutable.{ListBuffer, HashMap => HMap}
import utility.Stater
import io.MyTipsterStream
import scala.io.Source


/**
  * Created by andsora on 11/27/16.
  */


object PreProcessor {

  /** Add more rules of split
    * such as '-' and '-' are actually different even though they look the same
    *
    * @param text
    * @return
    */
  def tokenize(text: String) = {
    text.toLowerCase.split("[ .,;:?!*&$--+\"\'\t\n\r\f]+").filter(w => w.length >= 3).toList
  }

  /** First tokenize a string into a list of strings
    * Then, remove stop words and non-alphabetical words from a list of strings
    * SPECIAL RULES:
    * keep U.S.
    * turn presidentialcampaign into sperate words
    *
    * @param content
    * @return
    */
  def tokenWasher(content: String): List[String] = {
    val tokens = tokenWasher(tokenize(content)).toBuffer
    // Special rule
    val exceptionWords = List("U.S.")
    val replaceWords = Map("presidentialcampaign" -> List("presidential", "campaign"))
    exceptionWords.foreach{ word =>
      if (content.contains(word)) {
        tokens += word
      }
    }
    val tokensWithAdditionalWords = ListBuffer[String]()
    for (tk <- tokens) {
      if (replaceWords.contains(tk)) tokensWithAdditionalWords ++= replaceWords(tk)
      else tokensWithAdditionalWords += tk
    }
    tokensWithAdditionalWords.toList
  }

  /** Remove stop words and non-alphabetical words from a list of strings
    *
    * @param tokens
    * @return
    */
  def tokenWasher(tokens: List[String]): List[String] = {
    StopWords.filterOutSW(tokens)
              .filter(s => s.map(c => c.isLetter).reduce(_ && _)).toList
  }

  /** Iterate whole collection of documents and return a token map
    * which map a term to its id and frequency (in the whole collection)
    *
    * @param it
    * @param thres
    * @return
    */
  def getTokenMap(it: Iterator[Document], thres: Int): HMap[String, (Int, Int)] = {
    var termID = 0
    var times = 0
    val mm = HMap[String, Int]() // term -> term frequency
    val MM = HMap[String, (Int, Int)]() // term -> (term_id, term_freq)
    for (doc <- it) {
      if ({times += 1; times} % 100 == 0) {
        println("(getTokenMap) proccessed files: " + times)
      }
      // Consider both document content and title information
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

  /**
    *
    * @param it
    * @param TokenMap
    * @param ST
    */
  def getPostingsAndDocs(it: Iterator[Document], TokenMap: HMap[String, (Int, Int)],
                         ST: Stater): Tuple2[HMap[Int, ListBuffer[Int]], HMap[Int, Document]] = {
    var docID = 0
    var times = 0
    var postings = HMap[Int, ListBuffer[Int]]()
    var docs = HMap[Int, Document]()
    for (doc <- it) {
      // print the memory usage and time
      if ({times += 1; times} % 100 == 0) {
        println("(getPostings) processed files: " + times)
        if (times % 1000 == 0) {
          println("The size: " + postings.size)
          ST.PrintAll()
        }
      }

      // docID ++
      docID += 1

      // fill docs
      val prunedTokens = tokenWasher(doc.content).filter(token => TokenMap.contains(token))
      val prunedTitle = tokenWasher(doc.title).filter(token => TokenMap.contains(token))
      docs += docID -> new FeatureDocument(docID, doc.name, tf(prunedTokens, TokenMap), prunedTitle)

      // fill postings
      for (token <- prunedTokens ++ prunedTitle) {
        val termID = TokenMap(token)._1
        if (!postings.contains(termID)) postings += termID -> ListBuffer(docID)
        else if (postings(termID).last != docID) postings(termID) += docID
      }
    }
    (postings, docs)
  }

  /** Iterate the whole document and create a mutable hashmap
    * which maps a document id to the document
    *
    * @param it
    * @return
    */
  def getDocs(it: Iterator[Document]) = {
    val docs = HMap[Int, Document]()
    var docID = 0
    var times = 0
    for (doc <- it) {
      if ({times += 1; times} % 100 == 0) {
        println("(getDocs) proccessed files: " + times)
      }
      docs += {docID += 1; docID} -> doc
    }
    docs
  }

  /** Return term frequency of a given list of strings
    *
    * @param tokens
    * @param TokenMap
    * @return
    */
  def tf(tokens: List[String], TokenMap: HMap[String, (Int, Int)]): HMap[Int, Int] = {
    val mm = HMap[Int, Int]()
    for (token <- tokens) {
      val termID = TokenMap(token)._1
      if (!mm.contains(termID)) mm += termID -> 1
      else mm(termID) += 1
    }
    mm
  }

  /** Save resulting tokenmap
    *
    * @param dir
    * @param TokenMap
    */
  def saveTokenMap(dir: String, TokenMap: HMap[String, (Int, Int)]): Unit = {
    val file = new File(dir)
    val bw = new BufferedWriter(new FileWriter(file))
    // Write token map
    for (elem <- TokenMap) {
      bw.write(elem._1 + " " + elem._2._1 + " " + elem._2._2)
      bw.write("\n")
    }
    bw.close()

  }

  /** Save postings to files
    *
    * @param postings
    */
  def savePostings(dir: String, postings: HMap[Int, ListBuffer[Int]]) = {
    val file = new File(dir)
    val bw = new BufferedWriter(new FileWriter(file))
    // Write postins
    for (elem <- postings) {
      bw.write(elem._1 + "->" + elem._2.mkString(" "))
      bw.write("\n")
    }
    bw.close()
  }

  /** Load token map from dir
    *
    * @param dir
    * @return
    */
  def loadTokenMap(dir: String) = {
    val TokenMap = HMap[String, (Int, Int)]()
    val bufferedSource = Source.fromFile(dir)
    bufferedSource.getLines().foreach { line =>
      val Array(term, term_id, term_freq) = line.split(" ").map(_.trim())
      TokenMap += term -> (term_id.toInt, term_freq.toInt)
    }
    TokenMap
  }

  /** Load postings from dir
    *
    * @param dir
    */
  def loadPostings(dir: String) = {
    val postings = HMap[Int, List[Int]]()
    val bufferedSource = Source.fromFile(dir)
    bufferedSource.getLines().foreach { line =>
      val Array(term_id, doc_id_list) = line.split("->").map(_.trim())
      postings += term_id.toInt -> doc_id_list.split(" ").map(_.trim().toInt).toList
    }
    postings
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

    /*
    val It_1 = tips.stream.toIterator
    val TokenMap = getTokenMap(It_1, 10)
    println("The size of Map = " + TokenMap.size)
    ST.PrintAll()
    //println(TokenMap.filter(aa => aa._2._2 == 15))

    val It_2 = tips.stream.toIterator
    val result = getPostingsAndDocs(It_2, TokenMap, ST)
    val postings = result._1
    val docs = result._2
    */
//    saveTokenMap("data/tokenmap.txt", TokenMap)
//    savePostings("data/postings.txt", postings)
    val tokenmap = loadTokenMap("data/tokenmap.txt")
    val postings = loadPostings("data/postings.txt")
    println(tokenmap.take(10))
    println(postings.take(10))
    ST.PrintAll()
  }

}
