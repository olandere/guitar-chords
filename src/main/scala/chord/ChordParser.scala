package chord

/**
 * Created by eolander on 6/1/15.
 */

import grizzled.slf4j.Logging

import scala.language.postfixOps
import scala.util.parsing.combinator._

trait ChordParser extends RegexParsers {

  val root : Parser[String] = """[ABCDEFG][♯#b♭]?""".r ^^ {_.toString}
  val triad : Parser[String] = """m|-|\+|aug|dim|°""".r ^^ {_.toString}
  val quality: Parser[String] = """M|maj|Ma""".r ^^ {_.toString}
  val ext : Parser[String] = """6|7|9|11|13""".r ^^ {_.toString}
  val alteration:Parser[String] = """[♯#b♭](5|9|11|13)""".r ^^ {_.toString}
  val addedNote :Parser[String] = """add|/""".r ~> """[♯#b♭]?9|11|13""".r ^^ {_.toString}
  val suspension: Parser[String] = "sus" ~> """[24]""".r ^^ {_.toString}
  val noThird: Parser[String] = "no3".r ^^ {_.toString}
  val altRoot : Parser[String] = """/(?i)[ABCDEFG][♯#b♭]?""".r ^^ {_.tail}

  val sep:Parser[String] = """,|;|:|\s*""".r

  val pitch:Parser[Int] = """(\d|t|e)""".r ^^ {_.charAt(0) match {case 'e' => 11; case 't' => 10; case c => c.asDigit }}

  val pitchClass:Parser[List[Int]] = "{" ~> pitch.+ <~ "}" ^^ {pc => pc}

  val pitchClassWithRoot:Parser[Chord] = root.? ~ pitchClass ^^ {
    case r~pc => Chord(r, pc)
  }

 // val operation = """""".r ^^ {_.toString}

  val chord: Parser[Chord] = root ~ triad.? ~ quality.? ~ ext.? ~ addedNote.* ~ alteration.* ~ suspension.? ~
    noThird.? ~ altRoot.? ^^ {
    case r~t~q~e~ad~al~sus~no3~ar => Chord(r, t, e, q, al, ad, sus, ar)
  }

  val powerChord: Parser[Chord] = root <~ "5" ^^ (r => new PowerChord(Note(r)))

  val chordList: Parser[List[Chord]] = repsep(pitchClassWithRoot | powerChord | chord, sep)
}

object ChordParser extends ChordParser with Logging {

  def apply(input: String): List[Chord] = {
    parseAll(chordList, input) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        error(s"$input: ${failure.msg}")
        List(InvalidChord)
    }
  }
}
