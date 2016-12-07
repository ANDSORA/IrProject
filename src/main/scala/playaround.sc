import Preprocessing.FeatureDocument

import scala.io.Source

val line = "92;AP8802150113;-1;347->1 333->2 352->1 170->3 379->1 42->1 24->1 110->1 189->2 284->4 253->1 121->1 280->1 292->1 381->2 260->1 392->1 165->1 92->2 285->1 73->1 298->3 264->11 27->1 335->1 39->1 108->1 330->4 455->1 299->1 342->2 199->2 441->1 470->2 231->1 218->2 104->1 272->1 214->1 300->2 273->1 126->1 341->1 94->1 332->1 469->1 354->2 309->1 222->1 364->1"

val docs = collection.mutable.HashMap[Int, FeatureDocument]()

val Array(doc_id, doc_name, doc_title, doc_tf) = line.split(";").map(_.trim())

val title = doc_title.split(" ").toList.map(_.toInt)
val tf = doc_tf.split(" ")
  .map(_.trim).
  map(_.split("->").map(_.trim) match {
    case Array(term, term_freq) => (term.toInt, term_freq.toInt)
  }).toMap
docs += doc_id.toInt -> (new FeatureDocument(doc_id.toInt, doc_name, tf, title))

docs
