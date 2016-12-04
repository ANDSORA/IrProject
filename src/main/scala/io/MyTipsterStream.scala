package io

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.XMLDocument

/** Tipster stream that incorporate title information
  *
  * @param path
  * @param ext
  */
class MyTipsterStream(path: String, ext: String = "")
  extends TipsterStream(path: String, ext: String){
  override def stream: Stream[XMLDocument] = unparsed.stream.map(is => new MyTipsterParse(is))
}

object MyTipsterStream  {
  def main(args: Array[String]) {
    val tipster = new MyTipsterStream("/Users/Junlin/Documents/ETH/Information retrieval/Projects/Project_2/data/raw")
    println("Number of files in zips = " + tipster.length)
    val doc = tipster.stream.head
    println(doc.name + ", " + doc.title + ", " + doc.tokens)
    /*
    var length : Long = 0
    var tokens : Long = 0
    for (doc <- tipster.stream.take(10000)) {
      length += doc.content.length
      tokens += doc.tokens.length
    }
    println("Final number of characters = " + length)
    println("Final number of tokens     = " + tokens)
    */
  }
}
