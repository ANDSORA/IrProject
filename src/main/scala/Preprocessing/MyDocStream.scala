package Preprocessing

import ch.ethz.dal.tinyir.processing.Document

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MutMap}

/**
  * Created by andsora on 11/29/16.
  */
class MyDocStream (docs: Stream[Document]) {
  lazy val Name2ID: Map[String, Int] = docs.map(a => a.name).zipWithIndex.toMap
  lazy val ID2Name: Map[Int, String] = Name2ID.map(_.swap)

  private var ID: Int = 0
  /*
  lazy val stream: Stream[Document] = {
    val buffer = new ListBuffer[MyDocument]
    for (doc <- docs) {
      buffer += new MyDocument(Name2ID(doc.name), doc)
    }
    buffer.toStream
  }*/
  lazy val stream: Stream[Document] = docs.map(a => new MyDocument({ID += 1; ID}, a.name, a.content))


  /*
  val tokenMap: Map[String, Int] = {
    val mm = HashSet[String]()
    var id = 0
    for (doc <- stream) {
      for (token <- doc.tokens) {
        if (!mm.contains(token)) mm += token
      }
    }
    mm.toList.zipWithIndex.toMap
  }
  */
}