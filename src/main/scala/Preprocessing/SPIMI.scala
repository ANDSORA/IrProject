package Preprocessing

import java.io.{BufferedWriter, FileWriter}
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util
import ch.ethz.dal.tinyir.processing.StopWords
import collection.mutable.{HashMap => MutMap}
import io.MyTipsterStream
import utility.Stater

/**
  * Created by Junlin on 12/5/16.
  */

/** Implement Single Pass in Memory Indexing
  *
  */
case class SPIMI(val is: Iterator[XMLDocument], val dir: String) {
  val postings = MutMap[String, List[Int]]() // [term, List[(doc_no, term_freq_in_this_doc)]]

  def buildPosting = {
    // set state reporter
    val state = new Stater(new util.StopWatch, Runtime.getRuntime)
    state.start()
    var counter = 0
    var doc_id = 0
    for (doc <- is) {
      for (token <- doc.tokens) {
        postings += token -> (postings.getOrElse(token, List[Int]()) ++ List(doc_id))
      }
      counter += 1
      doc_id += 1
      if (counter % 2000 == 0) {state.PrintAll(); println(counter)}
    }
    postings
  }

  def buildTf(threshold: Int = 10) = {
    // set state reporter
    val state = new Stater(new util.StopWatch, Runtime.getRuntime)
    state.start()

    var tf = MutMap[String, Int]()
    var counter = 0
    for (doc <- is) {
      for (token <- doc.tokens) {
        if (!StopWords.stopWords.contains(token) && token.map(c => c.isLetter).reduce(_ && _))
          tf += token -> (tf.getOrElse(token, 0) + 1)
      }
      counter += 1
      if (counter % 2000 == 0) {state.PrintAll(); println(counter)}
    }
    val bw = new BufferedWriter(new FileWriter(dir))
    for (item <- tf) {
      if (item._2 < threshold) tf -= item._1
      else {
        bw.write(item._1 + " " + item._2)
        bw.write("\n")
      }
    }
    bw.close()
    tf
  }
}

object SPIMI extends App{
  println("Hello, IrProject.")

  // set the memory tracking
  val runtime = Runtime.getRuntime
  val tipster = new MyTipsterStream("data/raw")
  def stream = tipster.stream.toIterator
  val tf = SPIMI(stream, "data/dictionary.txt").buildTf(10)
  println(tf.toList.sortWith(_._2 > _._2).filter(_._2 < 15).take(500).mkString(", "))
//  val postings = SPIMI(stream).buildPosting
//  println(postings.take(100).mkString("\n"))
}
