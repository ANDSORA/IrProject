package models
import Preprocessing.{FeatureDocument, MyDocument, Query}
import ch.ethz.dal.tinyir.processing.Document
import com.sun.xml.internal.fastinfoset.vocab.Vocabulary

import scala.collection.mutable.{HashMap, LinkedHashMap, ListBuffer}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.math.log


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



class termbasedModel (postings: HashMap[Int, List[Int]], docs: HashMap[Int, FeatureDocument], queryTerm:List[Query]){
  val n = docs.size   // The size of the document
  val idf = postings.map{case (k,v) => (k,log(n/v.length))}                   // obtain a map termID => idf
  val tf = List[Int, HashMap[Int,Int]()]
  for (doc <- docs){
      val moretf = doc._2.tf.map{ case(k,v) => (k, log(1.0+v))}
    val tfidf = HMap[Int, Double]()
      for (eachtf <- moretf){
        if(idf.get(eachtf._1)!= None){
          tfidf += eachtf._1 -> eachtf._2 * idf(eachtf._1)
        }
      }
      }
      val tfidf = moretf.map{ case(k,v) => (k,v * idf(k))}
      tf += [doc._1]
  }




  val tfdoc = docs                // obtain a map from docID to tf
  val idf = postings.map{case (k,v) => (k,log(n/v.length))}                   // obtain a map termID => idf

  val tfidf = HashMap[Int, Double]()

   for (tf <- tfdoc){
      val realtf = tf._2  //finally obtain the tf for this document
      val readltf = realtf.map{ case(k,v) => (k,log(1.0+v)}   // obtain a ltf map termid => ltf
   }






  for (ltfdoc <- ltf ){
    ltfdoc._2
  }
  for (ltfa <- ltf){
    if(idf.get(ltfa._1)!= None){
      tfidf += ltfa._1 -> ltfa._2 * idf(ltfa._1)
    }
  }


  for (query <- queryTerm){



      val q = query.content  // The list of word in query
      for (doc <- docs){
        tfdoc
        for (qterm <- q){
          tfdoc += doc._2.tf.getOrElse(qterm,0.0)
        }
        // calculate the tf of each query word for each word
      }
  }



  val n = docs.size
  def getTfidf(TokenMap: HMap[String, (Int, Int)],postings: HMap[Int, ListBuffer[Int]], docs: HMap[Int, Document]):HMap[Int, Double]  ={
    val ltf = TokenMap.map{case (k,v) => (v._1, log(1.0+v._2))}  //obtain a map termID => ltf
    val idf = postings.map{case (k,v) => (k,log(n/v.length))}                   // obtain a map termID => idf

    val tfidf = HMap[Int, Double]()
    for (ltfa <- ltf){
      if(idf.get(ltfa._1)!= None){
        tfidf += ltfa._1 -> ltfa._2 * idf(ltfa._1)
      }
    }
    tfidf // termID -> tfidfweight
  }
}