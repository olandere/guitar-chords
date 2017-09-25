package chord

import grizzled.slf4j.Logging

import scala.util.parsing.combinator.RegexParsers

trait DegreeParser extends RegexParsers with Logging {

  val accidental = """[♯#b♭°]""".r ^^ {a => a}

  val deg = """[1]?[13]|[R12345679]""".r ^^ { d=>
    if (d == "R" || d == "1") 0 else d.toInt
  }

  val degree = accidental.? ~ deg ^^ {
    case a~d => Degree(d, a.map{Accidental(_)}.getOrElse(Natural))
  }
  val sep:Parser[String] = """,|;|:|\s*""".r

  val degreeList: Parser[List[Degree]] = repsep(degree, sep)
}

object DegreeParser extends DegreeParser {

  def apply(input: String): List[Degree] = {
    parseAll(degreeList, input) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        error(s"$input: ${failure.msg}")
        List(InvalidDegree)
    }
  }
}