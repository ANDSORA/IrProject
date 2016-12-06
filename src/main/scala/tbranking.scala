import scala.collection.mutable.{HashMap, ListBuffer}
import scala.collection.immutable.ListMap

import scala.collection.mutable.LinkedHashMap


object tbranking {
  def tbmodel(TokenMap: HashMap[String, Int],sample: List[(HashMap[Int, Double], Int)],query: List[String]): LinkedHashMap[Double,Int]= {
     var score = HashMap[Double,Int]()
     for( sam <- sample) {
        val tokenid = query.flatMap(q => TokenMap.get(q))
        val scoreOneDoc = tokenid.flatMap(t => sam._1.get(t)).sum
        score += scoreOneDoc -> sam._2
     }
    LinkedHashMap(score.toSeq.sortBy(_._1):_*).take(100)
  }



}