package Preprocessing

/**
  * Created by Junlin on 12/5/16.
  */

/** Query class that contains topic id and (preprocessed) content
  *
  * @param id
  * @param content: list of words represented by word_id
  */
case class Query(val id: Int, val content: List[Int]) {

  /** toString method
    *
    * @return
    */
  override def toString: String = {
    id + ": " + content.mkString(", ")
  }
}

object Query extends App {
  val q = Query(51, List(1,2))
  println(q)
}
