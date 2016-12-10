import scala.collection.mutable.{HashMap => MutHashMap}
import preprocessing.PreProcessor._
import preprocessing.{FeatureDocument, PreProcessor}
import preprocessing.TermFeature._

val vocabulary = MutHashMap("airbus" -> (1,2),"usa" -> (2,1),
  "france" -> (3,2),"eth" -> (4,2),
  "computer" -> (5,1),"science" -> (6,2))
val postings = MutHashMap(1 -> List(1,3), 2 -> List(1), 3 -> List(1, 3),
  4 -> List(2, 3), 5 -> List(2), 6 -> List(2, 3))
val ntopics = 2
val doc1       = new FeatureDocument(1, "doc_1", tf(tokenWasher("usa france airbus"), vocabulary))
val doc2       = new FeatureDocument(2, "doc_2", tf(tokenWasher("eth computer science"), vocabulary))
val doc3       = new FeatureDocument(3, "doc_3", tf(tokenWasher("airbus eth france science"),vocabulary))
val collection = MutHashMap(1 -> doc1, 2 -> doc2, 3 -> doc3)

PreProcessor.vocabularyPruner(vocabulary, postings, collection, 6)
