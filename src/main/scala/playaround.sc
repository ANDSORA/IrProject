import preprocessing.PreProcessor

val content = ""
PreProcessor.tokenWasher(content)

PreProcessor.ExceptionWords.foreach{ word =>
  if (content.contains(word)) println(word)
}