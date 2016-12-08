import Preprocessing.PreProcessor
import Preprocessing.PreProcessor.{tokenWasher, tokenize}

import scala.collection.mutable.ListBuffer

val content = "U.S., presidentialcampaign"

PreProcessor.tokenWasher(content)
