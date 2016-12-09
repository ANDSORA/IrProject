import preprocessing.PreProcessor

val content = "U.S., presidentialcampaign"
PreProcessor.tokenWasher(content)

PreProcessor.ExceptionWords.foreach{ word =>
  if (content.contains(word)) println(word)
}