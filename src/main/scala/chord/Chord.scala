//Ideas - handle altered tunings
//Ideas - handle chord progressions - find nearest chords
//todo - allow notes to be dropped - e.g. no 5
package chord

import scala.util.Try
import scalaz._, syntax.show._

class Chord(val root: String, val triad: String, val quality: String, val extension: Int, val alteration: String,
            val altRoot: Option[String]) {

  val INT_MAP = Map("1" -> 0, "3" -> 4, "5" -> 7, "6" -> 9, "7" -> 11, "9" -> 2, "11" -> 5, "13" -> 9)

  val tuning = List(0, 5, 10, 3, 7, 0)

//  def extensions = Range(9, extension + 1, 2).toList.map(_.toString)

  def intervals(extensions: => List[String] = Range(9, extension + 1, 2).toList.map(_.toString)): List[String] = {

    def performAlterations(ints: List[String]) = {
      def substitute(ints: List[String], alts: List[String]): List[String] = {
        println(s"ints: ${ints}, alts: ${alts}")
        if (alts.isEmpty) {ints} else if (ints.isEmpty) {alts} else {
          if (alts.head.endsWith(ints.head)) {
            alts.head :: substitute(ints.tail, alts.tail)
          } else {
            ints.head :: substitute(ints.tail, alts)
          }
        }
      }

      if (alteration.isEmpty) {
        ints
      } else {
        val altMatch = """([#b♯♭](5|9|11))""".r
        substitute(ints, altMatch.findAllIn(alteration).toList)
      }
    }

    val ints: List[String] = (List("1") :+ (triad match {
      case "min" | "dim" | "°" => "♭3"
      case _ => "3"
    }) :+ (triad match {
      case "dim" | "°" => "♭5"
      case "aug" => "♯5"
      case _ => "5"
    })) ++
                             (if (extension == 0) {
                               Nil
                             } else if (extension == 6) {
                               List("6")
                             } else {
                               if (quality == "maj") {
                                 List("7")
                               } else {
                                 if (quality == "dim" || quality == "°") {
                                   List("°7")
                                 } else if (quality != "add") {
                                   List("♭7")
                                 } else Nil
                               }
                             }) ++ (if (extension > 7 && quality != "add") {
      extensions
    } else if (extension > 7 && quality == "add") {
      List(extension.toString)
    } else {
      Nil
  })
    performAlterations(ints)
  }

  def semitones: List[Int] = intervals().map(INT_MAP.withDefault { i =>
    if (i.startsWith("b") || i.startsWith("♭")) {
      -1 + INT_MAP(i(1).toString)
    } else if (i.startsWith("°")) {
      -2 + INT_MAP(i(1).toString)
    }
    else {
      1 + INT_MAP(i(1).toString)
    }
                                                               })

//  def allChords(fretSpan: Int = 6): List[FretList] = {
//    def withinSpan(c: FretList): Boolean = {
//      val m = c.filter {_ != None}.map {_.get}
//      scala.math.abs(m.max - m.min) < fretSpan
//    }
//
//    def adjustOctave(c: FretList) = {
//      val m = c.filter {_ != None}.map {_.get}
//      if (m.min < 0) {
//        c.map {_.map { x: Int => x + 12}}
//      } else {
//        c
//      }
//    }
//
//    def transpose(c: FretList) =
//      c.map {_.map {_ + NOTE_MAP(root)}}
//
//
//    semitones
//    .map {Some(_)}
//    .padTo(6, None)
//    .permutations
//    .map(_.zip(tuning)).map {
//                              _.map { a: Tuple2[Option[Int], Int] => a._1.flatMap { x =>
//                                val d = x - a._2
//                                if (d < -6) {
//                                  Some(d + 12)
//                                } else if (d > 6) {
//                                  Some(d - 12)
//                                } else {
//                                  Some(d)
//                                }
//                                                                                  }
//                                    }
//                            }
//    .filter(withinSpan)
//    .toList
//    .map { c => adjustOctave(transpose(c))}
//    .sorted(fretListOrder.toScalaOrdering)
//  }

//  def printChords(fretSpan: Int = 6) = {
//    allChords(fretSpan).map(_.shows)
//  }

  def asDegrees(a: FretList) = {
    a.zip(tuning).map { case (f, s) => f.flatMap { n => Some(SEMI_TO_INT(norm(n + s - NOTE_MAP(root))))}
                      }.shows
  }

  //	 def diff(c: Chord, fretSpan: Int):Int = {
  //	 		for {
  // 		  c1 <- allChords(fretSpan)
  // 		  c2 <- c.allChords(fretSpan)
  // 		  diff()
  // 		}
  // 	}
  def diff(a: FretList, b: FretList): Int = {
    a.zip(b).map { e => if (e._1 == None || e._2 == None) {
      0
    } else {
      math.abs(e._1.get - e._2.get)
    }
                 }.foldLeft(0)(_ + _)
  }

  def asShell = new ShellChord(this)
}

object InvalidChord extends Chord("A", "", "", 0, "", None) {
  override def semitones = Nil
  override def intervals(extensions: => List[String]) = Nil
}

object Chord {

  val chordMatch = """([ABCDEFG][♯#b♭]?)(m|-|\+|aug|dim|°)?(M|maj|add)?(6|7|9|11|13)?(([♯#b♭](5|9|11))*)(/([ABCDEFG][♯#b♭]?))?"""
                   .r

  def triad(s: String) = {
    s match {
      case "m" | "-" => "min"
      case "aug" | "dim" | "°" => s
      case "+" => "aug"
      case _ => "maj"
    }
  }

  def seventh(t: String, s: String) = {
    t match {
      case "dim" | "°" => t
      case _ =>
        s match {
          case "M" | "maj" => "maj"
          case "add" => "add"
          case _ => "dom"
        }
    }
  }

  def apply(s: String) = {
    Try {
          val chordMatch(root, t, qual, ext, alt, _, _, _, altRoot) = s
          new Chord(root, triad(t), seventh(t, qual), Option(ext).getOrElse("0").toInt, Option(alt).getOrElse(""),
                    Option(altRoot))
        }.getOrElse(InvalidChord)
  }

  def unapply(s: String) = {
    s.split(" ").toList.map(c => c match {case "x" => None; case _ => Some(c.toInt)})
  }
}
