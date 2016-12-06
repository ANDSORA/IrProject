package utility


/**
  * Created by andsora on 12/6/16.
  */
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
