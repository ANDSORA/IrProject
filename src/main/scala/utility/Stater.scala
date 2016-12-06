package utility

import ch.ethz.dal.tinyir.util

/**
  * Created by Junlin on 12/5/16.
  */
class Stater(val sw: util.StopWatch, val runtime: Runtime) {
  val mb = 1024 * 1024

  def start(): Unit = {
    sw.start
  }

  def freeMeM() = runtime.freeMemory / mb

  def PrintMeM(): Unit = {
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
