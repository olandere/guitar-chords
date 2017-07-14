//Ideas - handle altered tunings
//Ideas - handle chord progressions - find nearest chords
//todo - allow notes to be dropped - e.g. no 5
package chord

class Chord(val root: Note, val triad: String, val quality: String, val extension: Int,
            val alteration: String, val added: List[String], val suspension: Option[String], val altRoot: Option[Note]) {

  def this(c: Chord) = {
    this(c.root, c.triad, c.quality, c.extension, c.alteration, c.added, c.suspension, c.altRoot)
  }

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case that: Chord => root == that.root && triad == that.triad && quality == that.quality &&
        extension == that.extension && alteration == that.alteration && added == that.added &&
        suspension == that.suspension && altRoot == that.altRoot
      case _ => false
    }

  def isValid: Boolean = true

  def isMinor: Boolean = triad == "min" || triad == "dim"

  val INT_MAP = Map("1" -> 0, "3" -> 4, "5" -> 7, "6" -> 9, "7" -> 11, "9" -> 2, "11" -> 5, "13" -> 9, "R" -> 0)

  def altRootInterval: Option[Int] =
    if (altRoot.isDefined) {
      Some(norm(NOTE_MAP(altRoot.get) - NOTE_MAP(root)))
    } else None

  /**
    * In case the chord has more tones than the instrument has strings, we need to split the semitones into
    * essential and non-essential intervals
    */
  def splitIntervals(strings: Int): (List[Int], List[Int]) = {
    if (strings > semitones.length) (semitones, Nil) else {
      (asShell.semitones, semitones.filterNot(i => asShell.semitones.contains(i)))
    }
  }

  def intervals(extensions: => List[String] = Range(9, extension + 1, 2).toList.map(_.toString)): List[String] = {

    def performAlterations(ints: List[String]) = {
      def substitute(ints: List[String], alts: List[String]): List[String] = {
        println(s"ints: $ints, alts: $alts")
        if (alts.isEmpty) {ints} else if (ints.isEmpty) {alts} else {
          if (alts.head.tail == ints.head) {
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
        substitute(ints, altMatch.findAllIn(alteration).toList)
      }
    }

    def performSuspensions(ints: List[String]) = {
      if (suspension.isEmpty) {
        ints
      } else {
        ints.map {
                     case "3" | "♭3" => suspension.map { case "2" => "9"
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
  }) ++ added
    performSuspensions(performAlterations(ints))
  }

  lazy val semitones: List[Int] = intervals().map(INT_MAP.withDefault { i =>
    if (i.startsWith("b") || i.startsWith("♭")) {
      -1 + INT_MAP(i.tail.toString)
    } else if (i.startsWith("°")) {
      -2 + INT_MAP(i.tail.toString)
    } else {
      1 + INT_MAP(i.tail.toString)
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

  def asSemi(a: FretList)(implicit tuning: Tuning): FretList = {
    a.zip(tuning.semitones).map { case (f, s) => f.map { n => norm(n + s - retune(tuning)(root))}}
  }

  def asDegrees(a: FretList)(implicit tuning: Tuning): DegreeList = {
    println(s"asDegrees($a) tuning: $tuning")
    val mapping = SEMI_TO_INT ++ semitones.zip(intervals()).toMap ++ (if (suspension.isDefined) Map(2 -> "2", 5 -> "4") else Map.empty)
    a.zip(tuning.semitones).map { case (f, s) => f.map { n => Degree(mapping(norm(n + s - retune(tuning)(root))))}
                      }
  }

//  def diff(c: Chord, fretSpan: Int): Int = {
//    for {
//      c1 <- allChords(fretSpan)
//      c2 <- c.allChords(fretSpan)
//      diff()
//    }
//  }

  def asShell: ShellChord = new ShellChord(this)

  def filterFingerings(fingerings: List[FretList]): List[FretList] = {
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
        case "dim" => ""
        case c => c
      })
    }
    root + q + (if (extension > 0) extension.toString else "") + added.fold(""){(r, a)=>r + s"add$a"} + alteration +
    suspension.fold(""){s=>s"sus$s"} +
    altRoot.map(r => s"/$r").getOrElse("")
  }
}

object InvalidChord extends Chord(InvalidNote, "", "", 0, "", Nil, None, None) {
  override lazy val semitones = Nil
  override def intervals(extensions: => List[String]): List[String] = Nil
  override def isValid: Boolean = false
}

class PowerChord(val r: Note) extends Chord(r, "", "", 0, "", Nil, None, None) {

  override lazy val semitones: List[Int] = List(0, 7)

  override def intervals(extensions: => List[String] = Nil): List[String] = List("R", "5")

  override def toString: String = r+"5"
}

object PowerChord {
  def apply(r: String) = new PowerChord(Note(r))
}

trait RootPosition {
  this: Chord =>
  override def filterFingerings(fingerings: List[FretList]): List[FretList] = {
    def isInRootPos(f: FretList) = {
      asDegrees(f).dropWhile(_.isEmpty).head.get.value == 0
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

  private val chordMatch = """([ABCDEFG][♯#b♭]?)(m|-|\+|aug|dim|°)?(M|maj)?(6|7|9|11|13)?(([♯#b♭](5|9|11))*)(add(9|11|13))?(sus(2|4))?(/([ABCDEFG][♯#b♭]?))?"""
                   .r

  private val powerChordMatch = """([ABCDEFG][♯#b♭]?)5""".r

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
    ChordParser(s).head
//    Try {
//          val chordMatch(root, t, qual, ext, alt, _, _, _, added, _, suspension, _, altRoot) = s
//          new Chord(s, root, triad(t), seventh(t, qual), Option(ext).getOrElse("0").toInt, Option(alt).getOrElse(""),
//                    Option(added), Option(suspension), Option(altRoot))
//        }.getOrElse(Try {
//          val powerChordMatch(root) = s
//          new PowerChord(s, root)
//        }.getOrElse(InvalidChord))
  }

  def apply(r: String, t: Option[String], e:Option[String],q:Option[String],al:List[String],ad:List[String],sus:Option[String],ar:Option[String]): Chord ={
    new Chord(Note(r), triad(t), seventh(t, q), e.getOrElse("0").toInt, al.mkString(""),
              ad,sus, ar.map(Note.apply))
  }

  def unapply(s: String)(implicit tuning: Tuning): FretList = {
    (if (s.trim.length == tuning.numStrings) {
      s.trim.toList.map{_.toString}
    } else {delimitedToList(s)})
    .map{case "x" | "X" => None; case c => Some(c.toInt)}
  }
}
