//Ideas - handle altered tunings
//Ideas - handle chord progressions - find nearest chords
//todo - allow notes to be dropped - e.g. no 5
package chord

import scala.util.Try

case class Chord(root: Note, triad: String, quality: String, extension: Int,
            alteration: String, added: List[String], suspension: Option[String], altRoot: Option[Note]) {

  def this(c: Chord) = {
    this(c.root, c.triad, c.quality, c.extension, c.alteration, c.added, c.suspension, c.altRoot)
  }

  def complexity: Int = {
    (if (triad.nonEmpty) 1 else 0) +
      (if (quality.nonEmpty) 1 else 0) +
      (if (extension > 0) 1 else 0) +
      (if (alteration.nonEmpty) 1 else 0) +
    added.length + suspension.map{_ => 1}.getOrElse(0) + altRoot.map{_ => 1}.getOrElse(0)
  }

  def inversions: List[List[Int]] = {
    val mod = new util.Modulo(12)
    (for {
      r <- semitones
      n <- semitones
    } yield mod(n - r)).grouped(semitones.length).toList.map{_.sorted}
  }

  def canEqual(a: Any): Boolean = a.isInstanceOf[Chord]

  override def equals(obj: scala.Any): Boolean =
    obj match {
//      case that: Chord => root.enhEquals(that.root) && triad == that.triad && quality == that.quality &&
//        extension == that.extension && alteration == that.alteration && added == that.added &&
//        suspension == that.suspension && altRoot == that.altRoot
      case that: Chord =>
        this.canEqual(that) && (
        if (root.enhEquals(that.root))
          semitones == that.semitones
        else if (isAug && that.isAug) {
          Set(Interval("M3"), Interval("m6"), Interval("A5"), Interval("d4")).contains(root.interval(that.root))
        } else false)
      case _ => false
    }

  def isValid: Boolean = true

  def isMinor: Boolean = triad == "min" || triad == "dim"

  def isAug: Boolean = this.semitones == List(0, 4, 8)

  def isPitchClassSet: Boolean = false

  val INT_MAP: Map[Degree, Int] = Map("1" -> 0, "3" -> 4, "5" -> 7, "6" -> 9, "7" -> 11, "9" -> 2, "11" -> 5, "13" -> 9, "R" -> 0).
    map{case (k, v) => Degree(k) -> v}

  def altRootInterval: Option[Int] =
    altRoot.map(ar => norm(NOTE_MAP(ar) - NOTE_MAP(root)))

  /**
    * In case the chord has more tones than the instrument has strings, we need to split the semitones into
    * essential and non-essential intervals
    */
  def splitIntervals(strings: Int): (List[Int], List[Int]) = {
    if (strings > semitones.length) (semitones, Nil) else {
      (asShell.semitones, semitones.filterNot(i => asShell.semitones.contains(i)))
    }
  }

  def intervals(extensions: => List[Degree] = Range(9, extension + 1, 2).toList.map{d => Degree(d.toString)}): List[Degree] = {

    def performAlterations(ints: List[Degree]) = {
      def substitute(ints: List[Degree], alts: List[Degree]): List[Degree] = {
//        println(s"ints: $ints, alts: $alts")
        if (alts.isEmpty) {ints} else if (ints.isEmpty) {alts} else {
          if (alts.head.value == ints.head.value) {
            alts.head :: substitute(ints.tail, alts.tail)
          } else {
            ints.head :: substitute(ints.tail, alts)
          }
        }
      }

      if (alteration.isEmpty) {
        ints
      } else {
        val altMatch = """([#b♯♭](5|9|11|13))""".r
        substitute(ints, altMatch.findAllIn(alteration).toList.map{a => Degree(a)})
      }
    }

    def performSuspensions(ints: List[Degree]): List[Degree] = {
      if (suspension.isEmpty) {
        ints
      } else {
        ints.map {
                     case Degree(3, Natural) | Degree(3, Flat) => suspension.map { case "2" => Degree("9")
                                                         case _ => Degree("11")
                                                       }.get
                     case x => x
                   }
        }
      }

    val ints: List[Degree] = (List(Degree("R")) :+ (triad match {
      case "min" | "dim" | "°" => Degree("♭3")
      case _ => Degree("3")
    }) :+ (triad match {
      case "dim" | "°" => Degree("♭5")
      case "aug" => Degree("♯5")
      case _ => Degree("5")
    })) ++
     (if (extension == 0) {
       Nil
     } else if (extension == 6) {
       List(Degree("6"))
     } else {
       if (quality == "maj") {
         List(Degree("7"))
       } else {
         if (quality == "dim" || quality == "°") {
           List(Degree("°7"))
         } else if (quality != "add") {
           List(Degree("♭7"))
         } else Nil
       }
     }) ++ (if (extension > 7 && quality != "add") {
      extensions
    } else {
      Nil
  }) ++ added.map{a => Degree(a)}
    performSuspensions(performAlterations(ints))
  }

  lazy val semitones: List[Int] = intervals().map(INT_MAP.withDefault { i =>
  //  println(s"i: $i")
    if (i.accidental == Flat) {
      -1 + INT_MAP(Degree(i.value, Natural))
    } else if (i.accidental == DoubleFlat || i.accidental == Dim) {
      -2 + INT_MAP(Degree(i.value, Natural))
    } else {
      1 + INT_MAP(Degree(i.value, Natural))
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

  lazy val notes: Seq[Note] = {
    val scale = Major(root)
    intervals().map(scale.noteFromDegree)
  }

  def asSemi(a: FretList)(implicit tuning: Tuning): FretList = {
    a.zip(tuning.semitones).map { case (f, s) => f.map { n => norm(n + s - retune(tuning)(root))}}
  }

  def asDegrees(a: FretList)(implicit tuning: Tuning): DegreeList = {
   // println(s"asDegrees($a) tuning: $tuning")
    val mapping = SEMI_TO_INT ++ semitones.zip(intervals()).toMap ++
      (if (suspension.isDefined) Map(2 -> Degree("2"), 5 -> Degree("4")) else Map.empty)
    val r = a.zip(tuning.semitones).map { case (f, s) => f.map { n => mapping(norm(n + s - retune(tuning)(root)))}
                      }
    r
  }

//  def diff(c: Chord, fretSpan: Int): Int = {
//    for {
//      c1 <- allChords(fretSpan)
//      c2 <- c.allChords(fretSpan)
//      diff()
//    }
//  }

  def asShell: ShellChord = new ShellChord(this)

  def filterFingerings(fingerings: List[FretList])(implicit tuning: Tuning): List[FretList] = {
    fingerings
  }

  override def toString: String = {
    def q = {
      (triad match {
        case "min" => "m"
        case "maj" => ""
        case "aug" => "+"
        case t => t
      }) + (quality match {
        case "dom" => ""
        case "maj" => "M"
        case "dim" | "°" => ""
        case c => c
      })
    }
    root + q + (if (extension > 0) extension.toString else "") + added.fold(""){(r, a)=>r + s"add$a"} + alteration +
    suspension.fold(""){s=>s"sus$s"} +
    altRoot.map(r => s"/$r").getOrElse("")
  }
}

object InvalidChord extends Chord(InvalidNote, "", "", 0, "", Nil, None, None) {
  override lazy val semitones: List[Int] = Nil
  override def intervals(extensions: => List[Degree]): List[Degree] = Nil
  override def isValid: Boolean = false
}

class PowerChord(val r: Note) extends Chord(r, "", "", 0, "", Nil, None, None) {

  override lazy val semitones: List[Int] = List(0, 7)

  override def intervals(extensions: => List[Degree] = Nil): List[Degree] = List(Degree("R"), Degree("5"))

  override def toString: String = r + "5"
}

object PowerChord {
  def apply(r: String): Chord = new PowerChord(Note(r))
}

class PitchClassSetChord(val r: String, val s: List[Int]) extends Chord(Note(r), "", "", 0, "", Nil, None, None) {
  override lazy val semitones: List[Int] = s

  override def toString: String = s"${if (r != "C") root else ""}{${s.map{case 11 => 'e'; case 10 => 't'; case c => c}.mkString(" ")}}"

  override def isPitchClassSet: Boolean = true

  override def intervals(extensions: => List[Degree]): List[Degree] = {
//    def helper(note: Note, s: List[Int]): List[Degree] = {
//      s.map(INT_MAP.map{case (k,v) => v -> k})
//
//    }
//    println(s"semitones: $semitones")
//    helper(root, semitones)

    semitones.map(SEMI_TO_INT)
  }

  override def asDegrees(a: FretList)(implicit tuning: Tuning): DegreeList = {
    val degrees = super.asDegrees(a)
    if (semitones.contains(6)) {
      if (semitones.contains(7)) {
        if (semitones.contains(5)) {
          listOptFunc.map(degrees)(d => if (d.semitone == 6) Degree("#11") else d)
        } else {
          listOptFunc.map(degrees)(d => if (d.semitone == 6) Degree("#4") else d)
        }
      } else {
        degrees
      }
    } else {
      degrees
    }
  }
}

trait RootPosition {
  this: Chord =>
  override def filterFingerings(fingerings: List[FretList])(implicit tuning: Tuning): List[FretList] = {
    def isInRootPos(f: FretList) = {
      asDegrees(f).dropWhile(_.isEmpty).head.get.value == 0
    }
    fingerings filter isInRootPos
  }
}

trait Drop2 {
  this: Chord =>
  override def filterFingerings(fingerings: List[FretList])(implicit tuning: Tuning): List[FretList] = {
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
  override def filterFingerings(fingerings: List[FretList])(implicit tuning: Tuning): List[FretList] = {
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

  def triad(s: String): String = {
    s match {
      case "m" | "-" => "min"
      case "aug" | "dim" | "°" => s
      case "+" => "aug"
      case _ => "maj"
    }
  }

  def triad(s: Option[String]): String = {
    s.map {
      case "m" | "-" => "min"
      case "aug" | "dim" | "°" => s.get
      case "+" => "aug"
      case _ => "maj"
    }.getOrElse("maj")
  }

  def seventh(t: String, s: String): String = {
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

  def seventh(t: Option[String], s: Option[String]): String = {
    t.flatMap {
      case "dim" | "°" => t
      case _ => None}.getOrElse {
        s.map {
          case "M" | "maj" => "maj"
          case "add" => "add"
          case _ => "dom"
        }.getOrElse("dom")
    }
  }

  def apply(s: String): Chord = {
    if (s.isEmpty) {
      InvalidChord
    } else {
      ChordParser(s).head
    }
  }

  def apply(r: String, t: Option[String], e:Option[String],q:Option[String],al:List[String],ad:List[String],sus:Option[String],ar:Option[String]): Chord ={
    new Chord(Note(r), triad(t), seventh(t, q), e.getOrElse("0").toInt, al.mkString(""),
              ad,sus, ar.map(Note.apply))
  }

  def apply(semitones: List[Int]): Chord = {
    new PitchClassSetChord("C", semitones)
  }

  def apply(r: Option[String], semitones: List[Int]): Chord = {
    new PitchClassSetChord(r.getOrElse("C"), semitones)
  }

  def unapply(s: String)(implicit tuning: Tuning): FretList = {
    if (s.trim.length < tuning.numStrings) {
      Nil
    } else Try(
    (if (s.trim.length == tuning.numStrings) {
      s.trim.toList.map{_.toString}
    } else {delimitedToList(s)})
    .map {
      case "x" | "X" => None
      case c => Some(c.toInt)
    }).getOrElse(Nil)
  }
}
