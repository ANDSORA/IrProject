/**
  * Created by Junlin on 11/29/16.
  */
package silenthinker.ir.math
import scala.util.Random

case class ProbVector (val arr: Array[Double]) extends AnyVal {

  def length = arr.length
  def operator(i: Int) : Double = arr(i)
  def update(i: Int, v: Double) = arr.update(i, v)

  def + (other: ProbVector) = combineVector(other,_+_)
  def - (other: ProbVector) = combineVector(other,_-_)
  def * (other: ProbVector) = combineVector(other,_*_)
  def / (other: ProbVector) = combineVector(other,_/_)

  def + (scalar: Double) = combineScalar(scalar,_+_)
  def - (scalar: Double) = combineScalar(scalar,_-_)
  def * (scalar: Double) = combineScalar(scalar,_*_)
  def / (scalar: Double) = { assert(scalar != 0.0); combineScalar(scalar,_/_) }

  def += (other: ProbVector) = updateVector(other, _+_)
  def -= (other: ProbVector) = updateVector(other, _-_)
  def *= (other: ProbVector) = updateVector(other, _*_)
  def /= (other: ProbVector) = updateVector(other, _/_)

  def += (scalar: Double) = updateScalar(scalar, _+_)
  def -= (scalar: Double) = updateScalar(scalar, _-_)
  def *= (scalar: Double) = updateScalar(scalar, _*_)
  def /= (scalar: Double) = updateScalar(scalar, _/_)

  def normalize : ProbVector = normalize(1.0)
  def normalize (s: Double): ProbVector = { updateScalar(s/arr.sum, _*_); this}


  def copy(other : ProbVector): ProbVector = {
    val newProb = ProbVector(new Array[Double](other.length))
    Array.copy(arr, 0, newProb.arr, 0, newProb.length)
    newProb
  }
  def mkString (sep: String): String = arr.mkString(sep)

  private def combineVector(other : ProbVector, func: (Double, Double) => Double) : ProbVector = {
    assert(arr.length == other.length)
    val result = new Array[Double](length)
    for (i <- arr.indices) result(i) = func(arr(i),other.arr(i))
    ProbVector(result)
  }
  private def updateVector(other : ProbVector, func: (Double, Double) => Double) : Unit = {
    assert(arr.length == other.length)
    for (i <- arr.indices) arr.update(i,func(arr(i),other.arr(i)))
  }

  private def combineScalar(scalar : Double, func: (Double, Double) => Double) : ProbVector = {
    val result = new Array[Double](length)
    for (i <- arr.indices) result(i) = func(arr(i),scalar)
    ProbVector(result)
  }

  private def updateScalar(scalar : Double, func: (Double, Double) => Double) : Unit =
    for (i <- arr.indices) arr(i) = func(arr(i),scalar)
}

object ProbVector {
  def random (n: Int, offset: Double = 100.0) : ProbVector =
    ProbVector(new Array[Double](n).map(v => offset+Random.nextDouble))

  def main(args: Array[String]): Unit = {
    val vec = ProbVector
   println(vec.random(3, 0.0).mkString(", "))
  }
}
