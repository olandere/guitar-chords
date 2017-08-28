package chord

import grizzled.slf4j.Logging

import scala.util.parsing.combinator.RegexParsers

trait DegreeParser extends RegexParsers with Logging {

  val degree: Parser[Degree] = """[♯#b♭]?[R234567]""".r ^^ {
    d => Degree(d)
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
        Nil
    }
  }
}