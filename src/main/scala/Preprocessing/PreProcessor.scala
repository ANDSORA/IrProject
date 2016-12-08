package Preprocessing

import java.io.{BufferedWriter, File, FileWriter}

import ch.ethz.dal.tinyir.processing.Document


import scala.collection.mutable.{HashMap, ListBuffer}
//import ch.ethz.dal.tinyir.processing.Tokenizer

import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.util.StopWatch

import scala.collection.mutable.{ListBuffer, HashMap => HMap}
import utility.Stater
import io.{MyCSVReader, MyTipsterStream}

import scala.io.Source
import scala.math.log

/**
  * Created by andsora on 11/27/16.
  */


object PreProcessor {

  val ExceptionWords: List[String] = List("U.S.")
  val ReplaceWords: Map[String, List[String]] = Map("presidentialcampaign" -> List("presidential", "campaign"))

  /** Add more rules of split
    * such as '-' and '-' are actually different even though they look the same
    *
    * @param text
    * @return
    */
  def tokenize(text: String) = {
    text.toLowerCase.split("[ .,;:?!*&$--+\"\'\t\n\r\f]+").filter(w => w.length >= 3).toList
  }

  /** First, tokenize a content string into a list of strings
    * Then, keep some ExceptionWords
    * Finally, escort the List of strings to lower tokenWasher
    *   for further operations
    *
    * @param content
    * @return
    */
  def tokenWasher(content: String): List[String] = {
    val tokens = tokenize(content).toBuffer
    ExceptionWords.foreach{ word =>
      if (content.contains(word)) tokens += word
    }
    tokenWasher(tokens.toList)
  }

  /** In additional to tokenWasher(content), filter words contained in dictionary
    *
    * @param content
    * @param TokenMap
    * @return
    */
  def tokenWasher(content: String, TokenMap: HMap[String, (Int, Int)]): List[String] = {
    val tokens = tokenWasher(content)
    tokens.filter(TokenMap.contains(_))
  }

  /** Remove stop words and non-alphabetical words from a list of tokenized strings
    * And then deal with the ReplaceWords
    *
    * @param tokens
    * @return
    */
  def tokenWasher(tokens: List[String]): List[String] = {
    // remove stop words and non-alphabetical words from a list of strings
    val Tokens = StopWords.filterOutSW(tokens)
              .filter(s => s.map(c => c.isLetter).reduce(_ && _)).toBuffer

    // deal with ReplaceWords
    val tokensWithAdditionalWords = ListBuffer[String]()
    for (tk <- Tokens) {
      if (ReplaceWords.contains(tk)) tokensWithAdditionalWords ++= ReplaceWords(tk)
      else tokensWithAdditionalWords += tk
    }
    tokensWithAdditionalWords.toList
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
                         ST: Stater): Tuple2[HMap[Int, ListBuffer[Int]], HMap[Int, FeatureDocument]] = {
    var docID = 0
    var times = 0
    var postings = HMap[Int, ListBuffer[Int]]()
    var docs = HMap[Int, FeatureDocument]()
    for (doc <- it) {
      // print the memory usage and time
      if ({times += 1; times} % 100 == 0) {
        println("(getPostingsAndDocs) processed files: " + times)
        if (times % 1000 == 0) {
          println("The size: " + postings.size)
          ST.PrintAll()
        }
      }
      if (!doc.content.isEmpty) { // only if doc has content
        // docID ++
        docID += 1
        // fill docs
        val prunedTokens = tokenWasher(doc.content, TokenMap)
        val prunedTitle = tokenWasher(doc.title, TokenMap)
        docs += docID -> new FeatureDocument(docID, doc.name, tf(prunedTokens, TokenMap), if (prunedTitle.isEmpty) List(-1) else prunedTitle.map(string2Id(_, TokenMap)))

        // fill postings
        for (token <- prunedTokens ++ prunedTitle) {
          val termID = string2Id(token, TokenMap)
          if (!postings.contains(termID)) postings += termID -> ListBuffer(docID)
          else if (postings(termID).last != docID) postings(termID) += docID
        }
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
      val termID = string2Id(token, TokenMap)
      if (!mm.contains(termID)) mm += termID -> 1
      else mm(termID) += 1
    }
    mm
  }
  /** Return tfidf weight of each term in each document
    *
    * @param postings
    * @param TokenMap
    * @return
    */
  def getTfidf(TokenMap: HMap[String, (Int, Int)],postings: HMap[Int, ListBuffer[Int]], docs: HMap[Int, Document]):HMap[Int, Double]  ={
    val ltf = TokenMap.map{case (k,v) => (v._1, log(1.0+v._2))}  //obtain a map termID => ltf
    val idf = postings.map{case (k,v) => (k,log(docs.size/v.length))}                   // obtain a map termID => idf

    val tfidf = HMap[Int, Double]()
    for (ltfa <- ltf){
         if(idf.get(ltfa._1)!= None){
             tfidf += ltfa._1 -> ltfa._2 * idf(ltfa._1)
         }
    }
    tfidf // termID -> tfidfweight
  }

  /** Turn String to id
    * Assume str is included in the dictionary
    * else return -1
    *
    * @param str
    * @param TokenMap
    * @return
    */
  def string2Id(str: String, TokenMap: HMap[String, (Int, Int)]): Int = {
    if (TokenMap.contains(str)) TokenMap(str)._1
    else -1
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

  /** Save preprocessed documents to files
    *
    * @param dir
    */
  def saveDocs(dir: String, docs: HMap[Int, FeatureDocument]) = {
    val sep = ";"
    def tf2String(tf: HMap[Int,Int]) = {
      tf.map{ case (term_id, term_freq) => (term_id.toString + "->" + term_freq.toString)}.mkString(" ")
    }
    val file = new File(dir)
    val bw = new BufferedWriter(new FileWriter(file))
    // Write docs
    for (elem <- docs) {
      bw.write(elem._1 + sep + elem._2.name + sep + elem._2.head.mkString(" ") + sep + tf2String(elem._2.tf))
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

  /** Load preprocessed docs from dir
    *
    * @param dir
    */
  def loadDocs(dir: String) = {
    val ST = new Stater(new StopWatch, Runtime.getRuntime)
    ST.start()
    val docs = HMap[Int, FeatureDocument]()
    val bufferedSource = Source.fromFile(dir)
    var counter = 0
    bufferedSource.getLines().foreach { line =>
      val splitLine = line.split(";").map(_.trim())
      if (splitLine.size == 4) {
        val Array(doc_id, doc_name, doc_title, doc_tf) = splitLine
        val title = doc_title.split(" ").toList.map(_.toInt)
        val _tf = doc_tf.split(" ")
          .map(_.trim).
          map(_.split("->").map(_.trim) match {
            case Array(term, term_freq) => (term.toInt, term_freq.toInt)
          }).toMap
        val tf = HMap[Int, Int]()
        for (item <- _tf) {
          tf += item._1 -> item._2
        }
        docs += doc_id.toInt -> (new FeatureDocument(doc_id.toInt, doc_name, tf, title))
      }
      else {
        val Array(doc_id, doc_name, doc_title) = splitLine
        val title = doc_title.split(" ").toList.map(_.toInt)
        docs += doc_id.toInt -> (new FeatureDocument(doc_id.toInt, doc_name, HMap[Int, Int](), title))
      }
      if (counter % 5000 == 0) {
        println(counter)
        ST.PrintAll()
      }
      counter += 1
    }
    docs
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
      val ST = new Stater(new StopWatch, Runtime.getRuntime)
      ST.start()

      val tips = new MyTipsterStream("data/raw")
      /*
      val It_1 = tips.stream.toIterator
      val TokenMap = getTokenMap(It_1, 10)
      println("The size of Map = " + TokenMap.size)
      ST.PrintAll()

      val It_2 = tips.stream.toIterator
      val result = getPostingsAndDocs(It_2, TokenMap, ST)
      val postings = result._1
      val docs = result._2
      saveDocs("data/docs.txt", docs)
      saveTokenMap("data/tokenmap.txt", TokenMap)
      savePostings("data/postings.txt", postings)
      */

      val TokenMap = loadTokenMap("data/tokenmap.txt")
      val postings = loadPostings("data/postings.txt")

      val docs = loadDocs("data/docs.txt")
      val emptyDoc = docs.filter(_._2.tf.isEmpty)
      val dir = "data/relevance-judgements.csv"
      val relevJudgement = MyCSVReader.loadRelevJudgement(dir)
      println(emptyDoc.map(_._2.name))
      println("*******************")
      println(emptyDoc.filter(item => relevJudgement.map(item => item._2).flatten.toList.contains(item._2.name)).map(_._2.name))

      println(TokenMap.size)
      println(postings.size)

      ST.PrintAll()
    }
}
