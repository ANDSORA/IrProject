package models
import Preprocessing.{MyDocument, Query}
import com.sun.xml.internal.fastinfoset.vocab.Vocabulary

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.collection.immutable.ListMap
import scala.collection.mutable.LinkedHashMap


/**
  * Created by ligua on 07.12.16.
  */
class tbModel  (TokenMap: HashMap[String, Int], tfidf: HashMap[Int, Double],q: Query) {
  var score = HashMap[Double,Int]()
  for( sam <- tfidf) {
    val tokenid = q.content.flatMap(q => TokenMap.get(q))
    val scoreOneDoc = tokenid.flatMap(t => sam._1.get(t)).sum
    score += scoreOneDoc -> sam._2
  }
  var ranking = LinkedHashMap(score.toSeq.sortBy(_._1):_*).take(100)
}
