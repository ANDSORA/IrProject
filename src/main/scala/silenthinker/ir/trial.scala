import silenthinker.ir.util.StopWatch
import silenthinker.ir.models.TopicModel

object trial extends App{

  val vocabulary = Set("a1","a2","a3","b1","b2","b3")
  val ntopics    = 2
  val model      = new TopicModel(vocabulary,ntopics)
  val doc1       = Map("a1" -> 1, "a2" ->1, "a3" ->1)
  val doc2       = Map("b1" -> 1, "b2" ->1, "b3" ->1)
  val doc3       = Map("a1" -> 1, "b2" ->1, "a3" ->1, "b1" ->1)
  val stream     = Stream(doc1, doc2, doc3)

  var count = 0.0
  val sw = new StopWatch
  for (i<- 0 until 50) model.learn(stream)
  println(sw.stopped)
  model.Pwt.foreach{ case (w,a) => println(w + ": " + a.mkString(" ")) }
  val query = List("a1", "a3")

  // Compute P(q|d) = \prod_d (1-lambda)*P(w|d)+lambda*P^top(w|d)

}

