package Indexing

import ch.ethz.dal.tinyir.indexing.{InvertedIndex, Result}
import ch.ethz.dal.tinyir.processing.Document

/**
  * Created by andsora on 11/29/16.
  */
case class FreqResult(val id: Int, val tf: List[Int]) extends Result[FreqResult] {
  def matches(that: FreqResult) = this.id compare that.id
  def matched(that: FreqResult) = FreqResult(id, this.tf ::: that.tf)
}

class MyFreqIndex (docs: Stream[Document]) extends InvertedIndex[FreqResult] {


  case class FreqPosting(val id: Int, val freq: Int) extends Ordered[FreqPosting] {
    def compare(that: FreqPosting) = this.id compare that.id
  }
  type PostList = List[FreqPosting]
  val index : Map[String,PostList] = {
    val groupedTuples = postings(docs).groupBy(_.term)
    groupedTuples.mapValues(_.map(tfT => FreqPosting(tfT.doc, tfT.count)).sorted)
  }

  case class TfTuple(term: String, doc: Int, count: Int)
  private def postings (s: Stream[Document]): List[TfTuple] =
    s.flatMap( d => d.tokens.groupBy(identity)
      .map{ case (tk,lst) => TfTuple(tk, d.ID, lst.length) } ).toList

  override def results (term: String) : List[FreqResult] =
    index.getOrElse(term,Nil).map(p => FreqResult(p.id, List(p.freq)))
}

/*
class MyFreqIndex (m: Int, docs: Stream[Document]) extends InvertedIndex[FreqResult] {


  case class FreqPosting(val id: Int, val freq: Int) extends Ordered[FreqPosting] {
    def compare(that: FreqPosting) = this.id compare that.id
  }
  type PostList = List[FreqPosting]
  val index : Map[String, String] = postings(docs).groupBy(_.term).mapValues(_.mkString(""))


  case class TfTuple(term: String, s: String)
  private def postings (s: Stream[Document]): List[TfTuple] =
    s.flatMap( d => d.tokens.groupBy(identity)
      .map{ case (tk,lst) => TfTuple(tk, tupleEncode(d.ID, lst.length) ) } ).toList

  override def results (term: String) : List[FreqResult] =
    if (index.contains(term)) tupleDecode(index(term)).map(p => FreqResult(p.id, List(p.freq))) else Nil
    //index.getOrElse(term,Nil).map(p => FreqResult(p.id, List(p.freq)))

  private def tupleEncode(id: Int, freq: Int): String = IntegerCompression.golombEncode(Array(id, freq), m)
  //private def tupleEncode(fr: FreqPosting): String = IntegerCompression.golombEncode(Array(fr.id, fr.freq), m)
  private def tupleEncode(lfr: List[FreqPosting]): String = lfr.map(fr => tupleEncode(fr.id,fr.freq)).mkString("")
  private def tupleDecode(byteStream: String): List[FreqPosting] = {
    val ret = ListBuffer[FreqPosting]()
    val IntRet = IntegerCompression.golombDecode(byteStream, m, Array[Int]())
    for (i <- 0 until IntRet.length / 2) {
      ret += FreqPosting(IntRet(2*i), IntRet(2*i + 1))
    }
    ret.toList
  }
}*/