/**
  * Created by andsora on 11/16/16.
  */
import ch.ethz.dal.tinyir.indexing.FreqIndex

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.StringDocument

object hello extends App {
  println("Hello, IrProject.")

  /*
  val tips = new TipsterStream("data/raw")
  var times = 0
  var tokenSet = HashSet[String]()
  for (doc <- tips.stream) {
    times += 1
    if(times % 100 == 0) {
      println(times + ": " + tokenSet.size)
    }
    for(token <- doc.tokens) {
      if(!tokenSet.contains(token)) tokenSet += token
    }
  }
  println("\nWe totally has " + times + " files.")
  println("We totally has " + tokenSet.size + " tokens.")
  */

  val d1 = new StringDocument(1,"mr sherlock holmes who was usually very late")
  val d0 = new StringDocument(0,"i can tell a moriaty when i see one said said holmes")
  val stream : Stream[StringDocument] = List(d1,d0).toStream
  val idx = new FreqIndex(stream)
  idx.index.foreach{ case (d,lst) => println(d + ": " + lst.mkString(" "))}
  val q = List("a","i")
  println(q.mkString(" ") + " = " + idx.results(q).mkString(" "))
  println(idx.results("said"))
}
