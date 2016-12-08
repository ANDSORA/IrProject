package models

import Preprocessing.{FeatureDocument, PreProcessor, Query}
import io.MyCSVReader
import Preprocessing.PreProcessor._
import ch.ethz.dal.tinyir.util.StopWatch
import utility.Stater

import collection.mutable.{HashSet, HashMap => MutHashMap}
import scala.collection.mutable
/**
  * Created by Junlin on 12/7/16.
  */

/** Search documents based on query and postings
  *
  * @param postings
  */
case class DocumentSearcher(val postings: MutHashMap[Int, List[Int]], val docs: MutHashMap[Int, FeatureDocument]) {

  /** Return related documents which contains at least one word of query
    *
    * @param q
    */
  def findRelatedDocuments(q: Query): Set[FeatureDocument] ={
    val relatedDocuments = mutable.HashSet[FeatureDocument]()
    q.content.foreach{w =>
      if (postings.contains(w)) relatedDocuments ++= docs.filter(item => postings(w).contains(item._1)).map(_._2)
    }
    relatedDocuments.toSet
  }
}

object DocumentSearcher extends App {
  val ST = new Stater(new StopWatch, Runtime.getRuntime)
  ST.start()
  val postings = loadPostings("data/postings.txt")
  ST.PrintAll()
  val TokenMap = PreProcessor.loadTokenMap("data/tokenmap.txt")
  ST.PrintAll()
  val docs = PreProcessor.loadDocs("data/docs.txt")
  val queries = MyCSVReader.loadQuery("data/questions-descriptions.txt")
  ST.PrintAll()
  val preprocessedQueries = queries.map(elem => new Query(elem._1,
    PreProcessor.tokenWasher(elem._2, TokenMap).map(PreProcessor.string2Id(_, TokenMap))))
  println(preprocessedQueries.head)
  println(DocumentSearcher(postings, docs).findRelatedDocuments(preprocessedQueries.head))
}
