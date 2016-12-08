package io

import java.io.InputStream

import ch.ethz.dal.tinyir.io.DocStream
import ch.ethz.dal.tinyir.processing.TipsterParse

/** Tipster parse that also gets title information
  * Sometimes no title/head information is provided, such as the case of AP880302-0275
  * Then title is an empty string.
  *
  * @param is: input stream
  */
class MyTipsterParse(is: InputStream) extends TipsterParse(is: InputStream){
  override def title   : String = {
    val t = read(doc.getElementsByTagName("HEAD"))
    if (t.isEmpty) read(doc.getElementsByTagName("SECOND"))
    else t
  }
}

object MyTipsterParse {
  def main(args: Array[String]) {
    val dirname = "/Users/Junlin/Documents/ETH/Information retrieval/Projects/Project_2/data/template"
    val fname = dirname + "/AP880215-0113.txt"
    val parse = new MyTipsterParse(DocStream.getStream(fname))
    val name = parse.name
    println(name)
    println("title: " + parse.title)
    val content = parse.content
    println(content.take(20) + "..." + content.takeRight(20))
  }
}
