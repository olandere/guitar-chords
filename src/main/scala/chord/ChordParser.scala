package chord

/**
 * Created by eolander on 6/1/15.
 */

import scala.language.postfixOps
import scala.util.parsing.combinator._

trait ChordParser extends RegexParsers {

//  def chord : Parser[Chord] =  """([ABCDEFG][♯#b♭]?)(m|-|\+|aug|dim|°)?(M|maj)?(6|7|9|11|13)?(([♯#b♭](5|9|11))*)(add(9|11|13))?(sus(2|4))?(/([ABCDEFG][♯#b♭]?))?"""
//               .r ^^ {c => Chord(c)}

  val root : Parser[String] = """[ABCDEFG][♯#b♭]?""".r ^^ {_.toString}
  val triad : Parser[String] = """m|-|\+|aug|dim|°""".r ^^ {_.toString}
  val quality: Parser[String] = """M|maj""".r ^^ {_.toString}
  val ext : Parser[String] = """6|7|9|11|13""".r ^^ {_.toString}
  val alteration:Parser[String] = """[♯#b♭](5|9|11)""".r ^^ {_.toString}
  val addedNote :Parser[String] = """add|/""".r ~> """[♯#b♭]?9|11|13""".r ^^ {_.toString}
  val suspension: Parser[String] = "sus" ~> """2|4""".r ^^ {_.toString}
  val altRoot : Parser[String] = """/[ABCDEFG][♯#b♭]?""".r ^^ {_.toString.tail}

  val sep:Parser[String] = """,|;|:|\s*""".r

  val operation = """""".r ^^ {_.toString}

  val chord: Parser[Chord] = root ~ triad.? ~ quality.? ~ ext.? ~ addedNote.? ~ alteration.* ~ suspension.? ~ altRoot.? ^^ {
    case r~t~q~e~ad~al~sus~ar => Chord(r, t, e, q, al, ad, sus, ar)
  }

  val powerChord: Parser[Chord] = root <~ "5" ^^ {
    case r => new PowerChord(r)
  }

  val chordList = repsep(powerChord | chord, sep)

}

object ChordParser extends ChordParser {

  def apply(input: String): List[Chord] = {
    parseAll(chordList, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }
}
