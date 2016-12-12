package utility

import scala.collection.mutable.ListBuffer

/**
  * Created by andsora on 12/11/16.
  */
object ListProcesser {
  def sortedListUnion(L1: List[Int], L2: List[Int]): List[Int] = {
    val LB = ListBuffer[Int]()
    var idx1 = 0
    var idx2 = 0
    while (idx1 < L1.length && idx2 < L2.length) {
      val t1 = L1(idx1)
      val t2 = L2(idx2)
      if (t1 < t2) {idx1 += 1; LB += t1}
      else if (t1 > t2) {idx2 += 1; LB += t2}
      else {idx1 += 1; idx2 += 1; LB += t1}
    }
    LB.toList ++ (L1.drop(idx1) ++ L2.drop(idx2))
  }

  def sortedArrayUnion(A1: Array[Int], A2: Array[Int]): List[Int] = {
    val LB = ListBuffer[Int]()
    var idx1 = 0
    var idx2 = 0
    while (idx1 < A1.length && idx2 < A2.length) {
      val t1 = A1(idx1)
      val t2 = A2(idx2)
      if (t1 < t2) {idx1 += 1; LB += t1}
      else if (t1 > t2) {idx2 += 1; LB += t2}
      else {idx1 += 1; idx2 += 1; LB += t1}
    }
    while (idx1 < A1.length) {LB += A1(idx1); idx1 += 1}
    while (idx2 < A2.length) {LB += A2(idx2); idx2 += 1}
    LB.toList
  }

  def sortedListIntersect(L1: List[Int], L2: List[Int]): List[Int] = {
    val LB = ListBuffer[Int]()
    var idx1 = 0
    var idx2 = 0
    while (idx1 < L1.length && idx2 < L2.length) {
      val t1 = L1(idx1)
      val t2 = L2(idx2)
      if (t1 < t2) {idx1 += 1}
      else if (t1 > t2) {idx2 += 1}
      else {idx1 += 1; idx2 += 1; LB += t1}
    }
    LB.toList
  }

  def main(args: Array[String]): Unit = {
    val L1 = List(1, 3, 5, 7)
    val L2 = List(2, 3, 6, 7, 8)
    println(sortedListIntersect(L1, L2))
    println(sortedArrayUnion(L1.toArray, L2.toArray))
  }
}
