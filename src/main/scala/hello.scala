/**
  * Created by andsora on 11/16/16.
  */
import ch.ethz.dal.tinyir.indexing.FreqIndex
import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch
import ch.ethz.dal.tinyir.compression.IntegerCompression

import Preprocessing.{MyDocStream, PreProcessor}

import scala.io.StdIn.readLine
import scala.util.control.Breaks.break
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.BitSet
import scala.collection.mutable.ArrayBuffer


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
  val TokenMap = PreProcessor.getTokenMap(tips.stream.toIterator, 10)
  state.PrintAll()
  println("\nTokenMap constructed.\n")

  // construct the Sample for training Model
  val Sample = ListBuffer[(HashMap[Int, Double], Int)]()
  //PreProcessor.getSample(tips.stream.toIterator, TokenMap, Sample)
  Sample.toList
  state.PrintAll()
  println("\nSample constructed.\n")
}
