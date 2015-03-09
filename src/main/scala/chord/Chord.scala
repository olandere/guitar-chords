//Ideas - handle altered tunings
//Ideas - handle chord progressions - find nearest chords
//todo - allow notes to be dropped - e.g. no 5
package chord

import scala.util.Try
import scalaz._, syntax.show._

class Chord(val name:String, val root: String, val triad: String, val quality: String, val extension: Int,
            val alteration: String, val added: Option[String], val suspension: Option[String], val altRoot: Option[String]) {

  def this(c: Chord) = {
    this(c.name, c.root, c.triad, c.quality, c.extension, c.alteration, c.added, c.suspension, c.altRoot)
  }

  val INT_MAP = Map("1" -> 0, "3" -> 4, "5" -> 7, "6" -> 9, "7" -> 11, "9" -> 2, "11" -> 5, "13" -> 9, "R" -> 0)

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

    def performSuspensions(ints: List[String]) = {
      if (suspension.isEmpty) {
        ints
      } else {
        ints.map { case "3" | "♭3" => suspension.map {
                                                       case "2" => "9"
                                                       case _ => "11"
                                                     }.get
                   case x => x
                 }
      }
    }

    val ints: List[String] = (List("R") :+ (triad match {
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
    } else {
      Nil
  }) ++ added.map(a=>List(a)).getOrElse(Nil)
    performSuspensions(performAlterations(ints))
  }

  lazy val semitones: List[Int] = intervals().map(INT_MAP.withDefault { i =>
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

  def asSemi(a: FretList)(implicit tuning: Tuning) = {
    a.zip(tuning.semitones).map { case (f, s) => f.map { n => norm(n + s - retune(tuning)(root))}}
  }

  def asDegrees(a: FretList)(implicit tuning: Tuning) = {
    val mapping = SEMI_TO_INT ++ semitones.zip(intervals()).toMap
    a.zip(tuning.semitones).map { case (f, s) => f.map { n => mapping(norm(n + s - retune(tuning)(root)))}
                      }
  }

  //	 def diff(c: Chord, fretSpan: Int):Int = {
  //	 		for {
  // 		  c1 <- allChords(fretSpan)
  // 		  c2 <- c.allChords(fretSpan)
  // 		  diff()
  // 		}
  // 	}

  def asShell = new ShellChord(this)

  def filterFingerings(fingerings: List[FretList]): List[FretList] = {
    fingerings
  }
}

object InvalidChord extends Chord("", "", "", "", 0, "", None, None, None) {
  override lazy val semitones = Nil
  override def intervals(extensions: => List[String]) = Nil
}

class PowerChord(val n:String, val r: String) extends Chord(n, r, "", "", 0, "", None, None, None) {

  override lazy val semitones: List[Int] = List(0, 7, 0)

  override def intervals(extensions: => List[String] = Nil) = List("R", "5", "R")
}

trait RootPosition {
  this: Chord =>
  override def filterFingerings(fingerings: List[FretList]): List[FretList] = {
    def isInRootPos(f: FretList) = {
      asDegrees(f).dropWhile(_.isEmpty).head.get == "R"
    }
    fingerings filter isInRootPos
  }
}

trait Drop2 {
  this: Chord =>
  override def filterFingerings(fingerings: List[FretList]): List[FretList] = {
    def isDrop2(frets: FretList) = {
      def consecutiveStrings =
        frets match {
          case List(None, None, _, _, _, _) => true
          case List(None, _, _, _, _, None) => true
          case List(_, _, _, _, None, None) => true
          case _ => false
        }
      def drop2Voicing = {
        val r::t::f::s::Nil = semitones
        asSemi(frets).filter(_.isDefined).map(_.get) match {
          case List(a, b, c, d) =>
            val d1 = math.abs(a - b)
            val d2 = math.abs(c - d)
            d1 == math.abs(r - f) || d2 == math.abs(r - f)
        }
      }
      consecutiveStrings && drop2Voicing
    }
    fingerings filter isDrop2
  }
}

trait Drop2and4 {
  this: Chord =>
  override def filterFingerings(fingerings: List[FretList]): List[FretList] = {
    def isDrop2and4(frets: FretList) = {
      def skipStrings =
        frets match {
          case List(None, _, _, None, _, _) => true
          case List(_, _,  None, _, _, None) => true
          case _ => false
        }
      def drop2Voicing = {
        val r::t::f::s::Nil = semitones
        asSemi(frets).filter(_.isDefined).map(_.get) match {
          case List(a, b, c, d) =>
            val d1 = math.abs(a - b)
            val d2 = math.abs(c - d)
            d1 == math.abs(r - f) || d2 == math.abs(r - f)
        }
      }
      skipStrings && drop2Voicing
    }
    fingerings filter isDrop2and4
  }
}

object Chord {

  val chordMatch = """([ABCDEFG][♯#b♭]?)(m|-|\+|aug|dim|°)?(M|maj)?(6|7|9|11|13)?(([♯#b♭](5|9|11))*)(add(9|11|13))?(sus(2|4))?(/([ABCDEFG][♯#b♭]?))?"""
                   .r

  val powerChordMatch = """([ABCDEFG][♯#b♭]?)5""".r

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
          val chordMatch(root, t, qual, ext, alt, _, _, _, added, _, suspension, _, altRoot) = s
          new Chord(s, root, triad(t), seventh(t, qual), Option(ext).getOrElse("0").toInt, Option(alt).getOrElse(""),
                    Option(added), Option(suspension), Option(altRoot))
        }.getOrElse(Try {
          val powerChordMatch(root) = s
          new PowerChord(s, root)
        }.getOrElse(InvalidChord))
  }

  def unapply(s: String) = {
    delimitedToList(s).map(c => c match {case "x" => None; case _ => Some(c.toInt)})
  }
}
