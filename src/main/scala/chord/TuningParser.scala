package chord

import grizzled.slf4j.Logging

import scala.language.postfixOps
import scala.util.parsing.combinator._

/**
  * Created by eolander on 4/23/16.
  */
trait TuningParser extends RegexParsers {

  val note: Parser[String] =
    """[ABCDEFG][♯#b♭]?""".r ^^ {
      _.toString
    }

  val fret: Parser[Int] =
    """1?\d""".r ^^ {
      _.toInt
    }

  val noteTuning: Parser[Tuning] = note.+ ^^ {ns => Tuning(ns)}

  val jmTuning: Parser[Tuning] = note ~ fret.* ^^ {
    case rt ~ fs => Tuning(fs.scanLeft(0) { (i, j) => mod12(i + j) }, Note(rt))
  }

  val tuning: Parser[Tuning] = noteTuning ||| jmTuning
}

object TuningParser extends TuningParser with Logging {

  import scala.collection.immutable.StringOps

  def capitalize(str: String): String = new StringOps(str).map(c => if ("acdefg".contains(c)) c.toUpper else c)

  def apply(input: String): Tuning = {
    parseAll(tuning, capitalize(input)) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        error(s"$input: ${failure.msg}")
        Tuning.StandardTuning
    }
  }
}
