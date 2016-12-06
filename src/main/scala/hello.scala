/**
  * Created by andsora on 11/16/16.
  */
import ch.ethz.dal.tinyir.indexing.FreqIndex
import ch.ethz.dal.tinyir.io.{DocStream, TipsterStream}
import ch.ethz.dal.tinyir.util.StopWatch
import ch.ethz.dal.tinyir.compression.IntegerCompression
import ch.ethz.dal.tinyir.processing._
import Preprocessing.{MyDocStream, PreProcessor}
import ch.ethz.dal.tinyir.processing.StopWords

import scala.io.StdIn.readLine
import scala.util.control.Breaks.break
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.BitSet
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.io.Source._
import javax.xml.parsers.DocumentBuilderFactory
import java.io.BufferedInputStream
import org.w3c.dom.{Document, NodeList}
import org.xml.sax.InputSource

class Stater(val sw: StopWatch, val runtime: Runtime) {
  def start(): Unit = {
    sw.start
  }

  def PrintMeM(): Unit = {
    val mb = 1024 * 1024
    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    println("** Free Memory:  " + runtime.freeMemory / mb)
    println("** Total Memory: " + runtime.totalMemory / mb)
    println("** Max Memory:   " + runtime.maxMemory / mb)
  }

  def PrintTime(): Unit = {
    println("** TIME:         " + sw.uptonow)
  }

  def PrintAll(): Unit = {
    println()
    PrintTime()
    println()
    PrintMeM()
  }
}

/*
class MyThread extends Runnable {
  def run: Unit = {
    /* memory info */
    val runtime = Runtime.getRuntime
    while (true) {
      Thread.sleep(1000)
      Stater.PrintMeM(runtime)
    }
  }
}
*/

object hello extends App {
  println("Hello, IrProject.")
 /*
  // set state reporter
  val state = new Stater(new StopWatch, Runtime.getRuntime)
  state.start()

  // set the memory tracking
  val runtime = Runtime.getRuntime

  // get the file stream and add ID, wash tokens
  val tips = new MyDocStream(new TipsterStream("data/raw").stream)
  state.PrintAll()
  println("\nFiles loaded.\n")

  // construct the TokenMap
  val TokenMap = HashMap[String, Int]()
  PreProcessor.getTokenMap(tips.stream, TokenMap)
  state.PrintAll()
  println("\nTokenMap constructed.\n")

  // construct the Sample for training Model
  val Sample = ListBuffer[(HashMap[Int, Double], Int)]()
  PreProcessor.getSample(tips.stream, TokenMap, Sample)
  Sample.toList
  state.PrintAll()
  println("\nSample constructed.\n")
  */



  val query = Source.fromFile("data/questions-descriptions.txt").getLines()
    .filter(_.contains("<title>")).map(s => s.replaceAll("<title>","")
    .replaceAll("Topic: ","")).replaceAll("[,;:?!*&$-+\"]"," ").replaceAll("\\*s+"," ").trim.toLowerCase)
                                                                                                                    //.replaceAll("""([\p{Punct}&&[^.]]|\b\p{IsLetter}{1,2}\b)""", "").trim)
  var n = 0
  for (a <- query){
    //a.map(_.trim)
    println(a)
    n += 1
    println(n)
  }


  println("\n"+n)
}
