/**
  * Created by andsora on 11/16/16.
  */

import scala.io.Source
import Preprocessing.PreProcessor
import scala.collection.mutable.{HashMap => HMap, ListBuffer, Map => MutMap}
import com.github.aztek.porterstemmer.PorterStemmer

/*
class MyThread extends Runnable {
  def run: Unit = {
    /* memory info */
    val runtime = Runtime.getRuntime
    while (true) {
      Thread.sleep(1000)
      utility.Stater.PrintMeM(runtime)
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


  val fname = "data/questions-descriptions.txt"

  PreProcessor.getQuery(fname).foreach(m => println(m._1+"  "+m._2))

 println("\n")

  val query = Source.fromFile("data/questions-descriptions.txt").getLines()
    .filter(_.contains("<title>")).map(s => s.replaceAll("<title>","")
    .replaceAll("Topic: ",""))//.trim.toLowerCase().split("[ /,;:?!*&$-+\"\'\t\n\r\f]+").toList.filter(!_.isEmpty))

  //replaceAll("[,;:?!*&$-+\"\\s+]", " "))//.replaceAll("\\s+", " ").trim.toLowerCase)


  val query2 = Source.fromFile("data/questions-descriptions.txt").getLines()
    .filter(_.contains("<title>")).map(s => s.replaceAll("<title>","")
    .replaceAll("Topic: ","").trim.toLowerCase().split("[ -/,;:?!*&$-+\"\t\n\r\f]+").filter(w => w.length >= 3).toList.filter(!_.isEmpty))

  //val queryterm  = query2.map(strList => PreProcessor.tokenWasher(strList.map(PorterStemmer.stem(_))))
  val queryterm2 = query2.map(strList => PreProcessor.tokenWasher(strList))
  var n = 0

  /*
  for (a <- queryterm2){
    //a.map(_.trim)
    println(a)
  //  println(b)
    n += 1
    println(n)
  }
  */
/*
  var m = 0
  for (a <- query2){
    //a.map(_.trim)
    println(a)
    m += 1
    println(m)
  }
*/

  val queryFinal = HMap[List[String],Int]()
  var ID = 50

  for (b <- queryterm2){
      queryFinal += b -> {ID += 1; ID}
  }

  queryFinal.foreach(m => println(m._1+"  "+m._2))




  println("\n"+n)



  // TODO ...
}
