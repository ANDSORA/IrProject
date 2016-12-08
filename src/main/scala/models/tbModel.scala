package models
import Preprocessing.{FeatureDocument, MyDocument, Query}
import ch.ethz.dal.tinyir.processing.Document
import com.sun.xml.internal.fastinfoset.vocab.Vocabulary

import scala.collection.mutable.{HashMap, LinkedHashMap, ListBuffer}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.math.log10


/**
  * Created by ligua on 07.12.16.
  */



class termbasedModel (postings: HashMap[Int, List[Int]], docs: HashMap[Int, FeatureDocument], queryTerm:List[Query]){
  def log2 ()


  val n = docs.size   // The size of the document
  val idf = postings.map{case (k,v) => (k,log(n/v.length))}                   // obtain a map termID => idf
  var tfidfall = ListBuffer[(Int, HashMap[Int,Double])]()            //List of tuples of (docID -> tfidf)
  for (doc <- docs){
      val moretf = doc._2.tf.map{ case(k,v) => (k, log(1.0+v))}
    val tfidf = HashMap[Int, Double]()
      for (eachtf <- moretf){
        if(idf.get(eachtf._1)!= None){
          tfidf += eachtf._1 -> eachtf._2 * idf(eachtf._1)
        }
      }
    tfidfall += Tuple2(doc._1,tfidf)
  }
  /*
  val sim = mutable.HashMap

  val qf = tfidfall.map{ case(k,v) => ()}

  for (query <- queryTerm){
    query.content


    //query.content
    val queryWeight = mutable.HashMap[Int,Double]()
    //val queryWei =
    for (doc <- docs){
       for (q <- query.content){

       }
    }

  }
  */




  var scoreFinal = ListBuffer[(Int, mutable.LinkedHashMap[Double,String])]()
  for (query <- queryTerm){
    var score = mutable.HashMap[Double,String]()
    for (eachtfidf <- tfidfall){
      val scoreDoc = query.content.flatMap(f => eachtfidf._2.get(f)).sum
      score += scoreDoc -> docs(eachtfidf._1).name              // The map from score -> docID
    }

    var ranking = LinkedHashMap(score.toSeq.sortBy(_._1):_*).take(100)     // SORT according to score of the document, obtain the query result for each query
    var docIDList = ListBuffer[String](C)
    for (eachRank <- ranking){
      docIDList += eachRank._2
    }
    scoreFinal += Tuple2(query.id, ranking)      // store each query result into a large variable
  }




}