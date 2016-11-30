package Preprocessing

import ch.ethz.dal.tinyir.processing.Document

/**
  * Created by andsora on 11/29/16.
  */
class MyDocument (id: Int, nam: String, txt: String) extends Document {
  def title = ""
  def body = ""
  def name = nam
  def date = ""
  def content = ""
  override def ID = id
  override def tokens = PreProcessor.tokenWasher(txt)
}
