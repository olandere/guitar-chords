package chord

import grizzled.slf4j.Logging

/**
 * Created by eolander on 2/17/15.
 */
class ChordNamer(val fl: FretList, val root: Note, val rootVal: Int, val alteredRoot: Option[Note] = None)(implicit tuning: Tuning) extends
  Logging {
  import ChordNamer._

 // println(s"fl: $fl, root: $root")

  private def SEMI_TO_DEGREE = Map(0 -> 0, 1 -> 2, 2 -> 2, 3 -> 3, 4 -> 3, 5 -> 4,
                           6 -> 5, 7 -> 5, 8 -> 6, 9 -> 6, 10 -> 7, 11 -> 7)

  lazy val intervals: FretList = {
    fl.zip(tuning.semitones).map{case (Some(n), t) => Some(norm(n+t-rootVal)); case (None, _) => None}//.filterNot(_ == None).map(_.get)//.sorted
  }

  private def hasMinorThird = intervals.contains(Some(3))

  private def hasMajorThird = intervals.contains(Some(4))

  private def hasSharpNine = hasMinorThird && hasMajorThird

  private def no3rd: Boolean = !(hasMinorThird || hasMajorThird)

  private def hasDomSeven = intervals.contains(Some(10))

  private def hasMajSeven = intervals.contains(Some(11))

  private def has7 = hasDomSeven || hasMajSeven

  private def hasDimSeventh = intervals.contains(Some(9))

  private def hasFlatFifth = intervals.contains(Some(6))

  private def hasFifth = intervals.contains(Some(7))

  private def hasAugFifth = intervals.contains(Some(8))

  private def isDim = hasMinorThird && hasFlatFifth

  def isDiminishedSeventh: Boolean = isDim && hasDimSeventh

  private def hasFlat9 = intervals.contains(Some(1))

  private def has9 = intervals.contains(Some(2))

  private def has11 = intervals.contains(Some(5))

  private def has13 = intervals.contains(Some(9))

  private def hasExtensions = has9 || has11 || has13

  private def isAugmented = hasMajorThird && hasAugFifth

  def tryAltRoot: Boolean = alteredRoot.isEmpty && no3rd && hasMajSeven

  def apply(): String = {
    if (isDiminishedSeventh) {
      "dim7"
    } else if (hasMinorThird) {
      "m" +
      (if (hasDomSeven)
         if (hasExtensions) {
           if (has13) "13" else if (has11) "11" else "9"
         } else "7" else "")
    } else if (hasMajorThird) {
      if (hasDomSeven) "7" else ""
    } else ""
  }

  def quality: String = {
    if (hasMajorThird) {
      if (hasAugFifth) {
        "+"
      } else {
        ""
      }
    } else if (hasMinorThird) {
      if (hasFlatFifth && !has7) {
        if (hasDimSeventh) {
          "dim7"
        } else {
          "dim"
        }
      } else {
        "m"
      }
    } else ""
  }

  private def intervalNumber: String = { //todo: handle suspensions
    def highestInt = {
      if (has13) {if (!has7) "6" else "13"} else
      if (has11 && !no3rd) "11" else if (has9 && !no3rd) "9" else "7"
    }
    if (!has7) "" else
    (if (hasMajSeven) {
      "M"
    } else "") + highestInt
  }

  private def addedIntervals: String = {
    if (!has7) {
      (if (has13 && !isDiminishedSeventh) {if (!has7) "6" else "13"} else "") +
      (if (has9 && !no3rd) "add9" else "") +
      (if (has11 && !no3rd && !alteredRoot.contains(Major(root).noteFromDegree(Degree("4")))) "add11" else "")
    } else ""
  }

  def determineInversion: String = {
    if (isDiminishedSeventh || isAugmented) {
    //  println("Is dim7")
      "root" //otherwise a diminished looks like 1st inversion
     } else {
    //  info(intervals.distinct)
    //  info(fretListToIntList(intervals.distinct).sorted map SEMI_TO_DEGREE)
    fretListToIntList(intervals.distinct).sorted map SEMI_TO_DEGREE match {
      case List(0, 3, 5) | List(0, 3, 5, 7) => "root" // case classes???
      case List(0, 3, 6) | List (0, 2, 3, 6) | List(0, 3, 5, 6) => "1st"
      case List(0, 4, 6) | List(0, 4, 6, 7) | List(0, 3, 4, 6) => "2nd"
      case List(0, 2, 4, 6) | List(0, 2, 5, 6) => "3rd" // try to handle tritone
//      case List(0, 3, 6) => "dim"
//      case List(0, 3, 6, 9) => "dim7"
//      case List(0, 4, 8) => "aug"

      case l => println(l); "root" // best guess
    }}
  }

  private def respell: ChordNamer = {
    def getRoot(fl: List[(Option[Int], Int)], chord: FretList, rtInt: Int): Int = {
      chord.head.collect {
        case x if SEMI_TO_DEGREE(x) == rtInt => norm(fl.head._1.get + fl.head._2)
      }.getOrElse(getRoot(fl.tail, chord.tail, rtInt))
    }

    def enhName(n: Note): Note = {
      //todo: need new intervals based on new root to determine whether this is a minor chord
      val scale = if (hasMinorThird) Aeolian(n) else Major(n)
      if (scale.isTheoretical) n.enharmonic else n
    }

    val newRoot = determineInversion match {
      case "root" => 0
      case "1st" => 6
      case "2nd" => 4
      case "3rd" => 2
      case _ => 0
    }
    val rootVal = getRoot(fl.zip(tuning.semitones), intervals, newRoot)
    val root = enhName(reverseNoteMap(tuning.root)(rootVal))

   // val ints = intervals(fl, root)
   // println(this)
    new ChordNamer(fl, root, rootVal, alteredRoot)
  }

  def alterations: String = {
    (if (hasFlatFifth && !isDiminishedSeventh) {
      if (hasFifth) "♯11" else {
        if (!isDim || has7) "♭5" else ""
      }
    } else "") +
      (if (hasFlat9) "♭9" else if (hasSharpNine) "♯9" else "")
  }

  private def suspension: String = {
    if (no3rd) {
      if (has9) "sus2" else if (has11) "sus4" else "no3"

    } else ""
  }

  override def toString: String = {
   // val preferSharps = Set("G", "D", "A", "E", "B").contains(root) || root.contains("♯")
    root + //reverseNoteMap(tuning.root)(root) +
    quality +
    intervalNumber +
    addedIntervals +
    alterations +
    suspension + (if (alteredRoot.isDefined) s"/${alteredRoot.get}" else "")
  }

  def chord: Chord = Chord(toString)
}

object ChordNamer {
//  def apply(chord: String) =
//    new ChordNamer(Chord.unapply(chord).map(_))()

  def fretListToIntList(fl: FretList): List[Int] = fl.flatMap{_.toList}.sorted

  def getRoot(fl: FretList, tuning: List[Int], tuningRoot: Note): (Note, Int) = {
    def enhName(n: Note): Note = {
      if (Major(n).isTheoretical) n.enharmonic else n
    }

    fl.head.map { root =>
      val rootVal = norm(root + tuning.head)
      (enhName(reverseNoteMap(tuningRoot)(rootVal)), rootVal)
    }.getOrElse(getRoot(fl.tail, tuning.tail, tuningRoot))
  }

//  def intervals(fl: FretList, root: Int) = {
//    fl.zip(tuning.semitones).map{case (Some(n), t) => Some(norm(n+t-root)); case (None, _) => None}//.filterNot(_ == None).map(_.get)//.sorted
//  }

  // accepts list of intervals
  //def apply(chord: List[Int]) = new ChordNamer(chord.sorted)()

  //accepts fingering
  def apply(fingering: String)(implicit tuning: Tuning): ChordNamer = {
    ChordNamer(Chord.unapply(fingering)).respell
  }

  def apply(fl: FretList)(implicit tuning: Tuning): ChordNamer = {

    val (root, rootVal) = getRoot(fl, tuning.semitones, tuning.root)
    //val ints = intervals(fl, root)
    new ChordNamer(fl, root, rootVal)//.respell //fretListToIntList(ints))
  }

  def asAlteredRoot(fingering: String)(implicit tuning: Tuning): ChordNamer = {
    //reverseNoteMap(tuning.root)(alteredRoot.get)
    def removeRoot(fl: FretList): FretList = fl.takeWhile(_.isEmpty) ++ List(None) ++ fl.dropWhile(_.isEmpty).tail

    val fl = Chord.unapply(fingering)
    val (altRoot, _) = getRoot(fl, tuning.semitones, tuning.root)
    val nfl = removeRoot(fl)
    val (newRoot, rootVal) = getRoot(nfl, tuning.semitones, tuning.root)
    new ChordNamer(fl, newRoot, rootVal, Option(altRoot)).respell

  }
}
