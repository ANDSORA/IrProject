package Preprocessing

import ch.ethz.dal.tinyir.processing.Document
import scala.collection.mutable.{HashMap => HMap}

/**
  * Created by andsora on 11/29/16.
  */

/** Document subclass that contains doc id, name, head/tile, and tokens
  *
  * @param id: user-assigned document id
  * @param nam: document name
  * @param txt: content
  * @param head: head/title of document, empty string by default
  */
class MyDocument (id: Int, nam: String, txt: String, head: String = "") extends Document {
  def title = head
  def body = ""
  def name = nam
  def date = ""
  def content = ""
  override def tokens = PreProcessor.tokenWasher(txt)
  override def ID = id

  /** toString method
    *
    * @return
    */
  override def toString(): String = {
    "**********" + "\n" +
    "ID: " + ID + ", Name: " + name + "\n" +
      "Title: " + title + "\n" +
      "Tokens: " + tokens + "\n" +
      "**********"
  }
}

class FeatureDocument (id: Int, nam: String, t: Map[Int,Int], hd: List[Int] = List()) extends Document {
  def title = ""
  def body = ""
  def name = nam
  def date = ""
  def content = ""
  def tf = t
  def head = hd
  override def ID = id

  override def toString(): String = {
    "**********" + "\n" +
      "ID: " + ID + ", Name: " + name + "\n" +
      "Title: " + head + "\n" +
      "Tokens: " + tf + "\n" +
      "**********"
  }
}
