/**
  * Created by andsora on 11/16/16.
  */
import ch.ethz.dal.tinyir.indexing.FreqIndex
import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.compression.IntegerCompression

import Preprocessing.{MyDocStream, PreProcessor}
import utility.{StopWatch, Stater}
import io.MyTipsterStream

import scala.io.StdIn.readLine
import scala.util.control.Breaks.break
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.{HashMap => HMap}
import scala.collection.mutable.BitSet
import scala.collection.mutable.ArrayBuffer

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
  val tips = new MyDocStream(new MyTipsterStream("data/raw").stream.take(10))
  state.PrintAll()
  println("\nFiles loaded.\n")

  // construct the TokenMap
  val TokenMap = PreProcessor.getTokenMap(tips.stream.toIterator, 10)
  state.PrintAll()
  println("\nTokenMap constructed.\n")

  // construct the Sample for training Model
  val Sample = ListBuffer[(HMap[Int, Double], Int)]()
  //PreProcessor.getSample(tips.stream.toIterator, TokenMap, Sample)
  Sample.toList
  state.PrintAll()
  println("\nSample constructed.\n")
}
