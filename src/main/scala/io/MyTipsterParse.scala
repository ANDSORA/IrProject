package io

import java.io.InputStream

import ch.ethz.dal.tinyir.io.DocStream
import ch.ethz.dal.tinyir.processing.TipsterParse

/** Tipster parse that also gets title information
  *
  * @param is: input stream
  */
class MyTipsterParse(is: InputStream) extends TipsterParse(is: InputStream){
  override def title   : String = read(doc.getElementsByTagName("HEAD"))
}

object MyTipsterParse {
  def main(args: Array[String]) {
    val dirname = "/Users/Junlin/Documents/ETH/Information retrieval/Projects/Project_2/data"
    val fname = dirname + "/AP880301-0271 template.txt"
    val parse = new MyTipsterParse(DocStream.getStream(fname))
    val name = parse.name
    println(name)
    println(parse.title)
    val content = parse.content
    println(content.take(20) + "..." + content.takeRight(20))
  }
}
