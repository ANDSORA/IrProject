import Preprocessing.PreProcessor._
import Preprocessing.{PreProcessor, Query}
import io.MyCSVReader

import scala.collection.mutable

val dir = "/Users/Junlin/Documents/ETH/Information retrieval/Projects/Project_2/code/data"
val TokenMap = PreProcessor.loadTokenMap(dir + "/tokenmap.txt")
val postings = loadPostings(dir + "/postings.txt")

val csvReader = new MyCSVReader
val relevJudgement = csvReader.loadRelevJudgement(dir + "/relevance-judgements.csv")
val queries = csvReader.loadQuery(dir + "/questions-descriptions.txt")
val preprocessedQueries = queries.map(elem => new Query(elem._1,
  PreProcessor.tokenWasher(elem._2, TokenMap).map(PreProcessor.string2Id(_, TokenMap))))

val q = preprocessedQueries.head
/*
val relatedDocuments = mutable.HashSet[Int]()
q.content.foreach{w =>
  if (postings.contains(w)) relatedDocuments ++= postings(w).toSet
}
relatedDocuments.toSet
*/