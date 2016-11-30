/**
  * Created by andsora on 11/16/16.
  */
import ch.ethz.dal.tinyir.indexing.FreqIndex
import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch
import ch.ethz.dal.tinyir.compression.IntegerCompression

import Preprocessing.{MyDocStream, MyFreqIndex, PreProcessor}

import scala.io.StdIn.readLine
import scala.util.control.Breaks.break
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.BitSet
import scala.collection.mutable.ArrayBuffer

class MyThread extends Runnable {
  def run: Unit = {
    /* memory info */
    val mb = 1024 * 1024
    val runtime = Runtime.getRuntime
    while (true) {
      Thread.sleep(1000)
      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
      println("** Free Memory:  " + runtime.freeMemory / mb)
      println("** Total Memory: " + runtime.totalMemory / mb)
      println("** Max Memory:   " + runtime.maxMemory / mb)
      println()
    }
  }
}

object hello extends App {
  println("Hello, IrProject.")

  val sw = new StopWatch
  sw.start

  val newThread = new Thread(new MyThread)
  newThread.start()

  //val tips = new TipsterStream("data/raw")
  val mytips = new MyDocStream(new TipsterStream("data/raw").stream)

  println("PreParation is done!")

  val tkm = mytips.stream.flatMap( d => d.tokens.groupBy(identity)
    .map{ case (tk, lst) => (mytips.tokenMap(tk), d.ID, lst.length)} ).groupBy(_._1).mapValues(_.map(a => (a._2, a._3)))

  var num = 0
  for (item <- tkm) {
    num += 1
  }

  println("num of item: " + num)

  /*
  val idx1 = new FreqIndex(mytips.stream.take(5000))
  val idx2 = new MyFreqIndex(100, mytips.stream.take(5000))
  println("idx1.index.size: " + idx1.index.size)
  println("idx2.index.size: " + idx2.index.size)

  val s1 = idx1.index.map(a => a._2.length).sum
  val s2 = idx2.index.map(a => a._2.length).sum

  println("idx1.size: " + s1)
  println("idx2.size: " + s2)
  */

  /*
  while (true) {
    val token: String = readLine()
    if (idx.index.contains(token))
      println(idx.index(token))
    else
      println("Sorry, Invalid token : (")
  }
  */

  /*
  val TokenSet = HashSet[String]()
  val WashedTokenSet = HashSet[String]()

  var times = 0

  for (doc <- tips.stream.take(50000)) {
    times += 1
    if (times % 100 == 0) {
      println("times: " + times)
      println("tokenSet.size = " + TokenSet.size)
    }

    for (s <- doc.tokens) {
      if (!TokenSet.contains(s)) TokenSet += s
    }
    /*
    for (s <- PreProcessor.tokenWasher(doc.tokens)) {
      if (!WashedTokenSet.contains(s)) WashedTokenSet += s
    }*/
  }
  //
  println("\ntokenSet.size = " + TokenSet.size)
  println("washedTokenSet.size = " + WashedTokenSet.size)
  */

  newThread.stop()

  val runtime = Runtime.getRuntime
  val mb = 1024 * 1024

  println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
  println("** Free Memory:  " + runtime.freeMemory / mb)
  println("** Total Memory: " + runtime.totalMemory / mb)
  println("** Max Memory:   " + runtime.maxMemory / mb)

  println("(time): " + sw.uptonow)
  println("work is done!")

  /*
  val b = BitSet(1,3,8)
  println("BitSets: "+b+" = "+IntegerCompression.byteToString(b)+" = "+IntegerCompression.binaryStringToBitSet(IntegerCompression.byteToString(b)))
  println("VB code: ("+Array(1,2,3,130).mkString(", ")+") = "+IntegerCompression.bytesToString(IntegerCompression.VBEncode(Array(1,2,3,130)))+" = ("+IntegerCompression.VBDecode(IntegerCompression.VBEncode(Array(1,2,3,130)),0,Array[Int]()).mkString(", ")+")")
  println("Unary code: ("+Array(1,2,3,130).mkString(", ")+") = "+Array(1,2,3,130).map(f => IntegerCompression.intToUnary(f)).mkString("")+" = ("+IntegerCompression.unaryToInt(Array(1,2,3,130).map(f => IntegerCompression.intToUnary(f)).mkString("")).mkString(", ")+")")
  println("Gamma code: ("+Array(1,2,3,130).mkString(", ")+") = "+IntegerCompression.gammaEncode(Array(1,2,3,130))+" = ("+IntegerCompression.gammaDecode(IntegerCompression.gammaEncode(Array(1,2,3,130)),Array[Int]()).mkString(", ")+")")
  println("Golomb code: ("+Array(1,2,3,130).mkString(", ")+") = "+IntegerCompression.golombEncode(Array(1,2,3,130),10)+" = ("+IntegerCompression.golombDecode(IntegerCompression.golombEncode(Array(1,2,3,130),10), 10, Array[Int]()).mkString(", ")+")")
  */
  /*
  val d1 = new StringDocument(1,"mr sherlock holmes who was usually very late")
  val d0 = new StringDocument(0,"i can tell a moriaty when i see one said said holmes")
  val stream : Stream[StringDocument] = List(d1,d0).toStream
  val idx = new FreqIndex(stream)
  idx.index.foreach{ case (d,lst) => println(d + ": " + lst.mkString(" "))}
  val q = List("a","i")
  println(q.mkString(" ") + " = " + idx.results(q).mkString(" "))
  idx.index("holmes").map(a => println(a.freq))
  */
}
