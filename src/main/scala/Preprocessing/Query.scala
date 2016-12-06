package Preprocessing

/**
  * Created by Junlin on 12/5/16.
  */

/** Query class that contains topic id and (tokenized) content
  *
  * @param id
  * @param content: list of strings that are already tokenized, token-washed!
  */
case class Query(val id: Int, val content: List[String]) {

  /** toString method
    *
    * @return
    */
  override def toString: String = {
    id + ": " + content.mkString(", ")
  }
}

object Query extends App {
  val q = Query(51, List("airbus", "subsidies"))
  println(q)
}
