package Preprocessing

import ch.ethz.dal.tinyir.processing.StopWords
import ch.ethz.dal.tinyir.processing.Tokenizer
import com.github.aztek.porterstemmer.PorterStemmer


/**
  * Created by andsora on 11/27/16.
  */

object PreProcessor {
  def tokenWasher(content: String): List[String] = tokenWasher(Tokenizer.tokenize(content))
  def tokenWasher(tokens: List[String]): List[String] = {
    StopWords.filterOutSW(tokens.map(a => PorterStemmer.stem(a)))
              .filter(s => s.map(c => c.isLetter).reduce(_ && _)).toList
  }
}
