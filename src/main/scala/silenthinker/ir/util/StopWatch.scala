package silenthinker.ir.util
import scala.collection.mutable.ArrayBuffer

class StopWatch {

  private var startTime : Long = 0
  private val stopTimes  = new ArrayBuffer[Long]

  final def start : Unit   = { startTime = System.nanoTime; stopTimes.clear }
  final def stop  : Long   = {
    val now = System.nanoTime
    stopTimes += (now-startTime)
    stopTimes.last
  }
  def uptonow     : Double = { (System.nanoTime - startTime).toDouble/1e9 }
  def stopped     : String = { "in " + stop.toDouble/1e9 + " sec." }
  def allStopped  : String = { stopTimes.toString }
}

object StopWatch extends App{
  val sw = new StopWatch
  sw.start
  println(sw.stopped)
}