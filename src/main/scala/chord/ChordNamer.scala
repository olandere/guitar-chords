package chord

import grizzled.slf4j.Logging

/**
 * Created by eolander on 2/17/15.
 */
class ChordNamer(val fl: FretList, val root: Int)(implicit tuning: Tuning) extends Logging {
  import ChordNamer._

 // println(s"fl: $fl, root: $root")

  def SEMI_TO_DEGREE = Map(0 -> 0, 1 -> 2, 2 -> 2, 3 -> 3, 4 -> 3, 5 -> 4,
                           6 -> 5, 7 -> 5, 8 -> 6, 9 -> 6, 10 -> 7, 11 -> 7)

  lazy val intervals: FretList = {
    fl.zip(tuning.semitones).map{case (Some(n), t) => Some(norm(n+t-root)); case (None, _) => None}//.filterNot(_ == None).map(_.get)//.sorted
  }

  private def hasMinorThird = intervals.contains(Some(3))

  private def hasMajorThird = intervals.contains(Some(4))

  private def hasSharpNine = hasMinorThird && hasMajorThird

  private def no3rd = !(hasMinorThird || hasMajorThird)

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

  def apply(): String = {
    if (isDiminishedSeventh) "dim7"
    else if (hasMinorThird) {
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
      (if (has11 && !no3rd) "add11" else "")
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
      case List(0, 3, 6) | List(0, 3, 5, 6) => "1st"
      case List(0, 4, 6) | List(0, 3, 4, 6) => "2nd"
      case List(0, 2, 4, 6) | List(0, 2, 5, 6) => "3rd" // try to handle tritone
//      case List(0, 3, 6) => "dim"
//      case List(0, 3, 6, 9) => "dim7"
//      case List(0, 4, 8) => "aug"

      case l => println(l); "root" // best guess
    }}
  }

  private def respell: ChordNamer = {
    def getRoot(fl: List[(Option[Int], Int)], chord: FretList, rtInt: Int): Int = {
      if (chord.head.isDefined && SEMI_TO_DEGREE(chord.head.get) == rtInt) {
        norm(fl.head._1.get + fl.head._2)
      } else {
        getRoot(fl.tail, chord.tail, rtInt)
      }
    }

    val newRoot = determineInversion match {
      case "root" => 0
      case "1st" => 6
      case "2nd" => 4
      case "3rd" => 2
      case _ => 0
    }
    val root = getRoot(fl.zip(tuning.semitones), intervals, newRoot)
   // val ints = intervals(fl, root)
   // println(this)
    new ChordNamer(fl, root)
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
      if (has9) "sus2" else if (has11) "sus4" else ""

    } else ""
  }

  override def toString: String = {
   // val preferSharps = Set("G", "D", "A", "E", "B").contains(root) || root.contains("♯")
    reverseNoteMap(tuning.root)(root) +
    quality +
    intervalNumber +
    addedIntervals +
    alterations +
    suspension
  }
}

object ChordNamer {
//  def apply(chord: String) =
//    new ChordNamer(Chord.unapply(chord).map(_))()

  def fretListToIntList(fl: FretList): List[Int] = fl.flatMap{_.toList}.sorted

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
    def getRoot(fl: FretList, tuning: List[Int]): Int = {
      if (fl.head.isDefined) norm(fl.head.get+tuning.head) else getRoot(fl.tail, tuning.tail)
    }
    val root = getRoot(fl, tuning.semitones)
    //val ints = intervals(fl, root)
    new ChordNamer(fl, root)//.respell //fretListToIntList(ints))
  }
}
