package chord

/**
 * Created by eolander on 2/17/15.
 */
class ChordNamer(val fl: FretList, val root: Int)(implicit tuning: Tuning)  {
  import ChordNamer._

  println(s"fl: $fl, root: $root")

  def SEMI_TO_DEGREE = Map(0 -> 0, 1 -> 2, 2 -> 2, 3 -> 3, 4 -> 3, 5 -> 4,
                           6 -> 5, 7 -> 5, 8 -> 6, 9 -> 6, 10 -> 7, 11 -> 7)

  lazy val intervals = {
    fl.zip(tuning.semitones).map{case (Some(n), t) => Some(norm(n+t-root)); case (None, _) => None}//.filterNot(_ == None).map(_.get)//.sorted
  }

  def hasMinorThird = intervals.contains(Some(3))

  def hasMajorThird = intervals.contains(Some(4))

  def no3rd = !(hasMinorThird || hasMajorThird)

  def hasDomSeven = intervals.contains(Some(10))

  def hasMajSeven = intervals.contains(Some(11))

  def has7 = hasDomSeven || hasMajSeven

  def hasDimSeventh = intervals.contains(Some(9))

  def hasFlatFifth = intervals.contains(Some(6))

  def hasFifth = intervals.contains(Some(7))

  def hasAugFifth = intervals.contains(Some(8))

  def isDiminishedSeventh = hasMinorThird && hasFlatFifth && hasDimSeventh

  def hasFlat9 = intervals.contains(Some(1))

  def has9 = intervals.contains(Some(2))

  def has11 = intervals.contains(Some(5))

  def has13 = intervals.contains(Some(9))

  def hasExtensions = has9 || has11 || has13

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

  def quality = {
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

  def intervalNumber = { //todo: handle suspensions
    def highestInt = {
      if (has13) "13" else if (has11 && !no3rd) "11" else if (has9 && !no3rd) "9" else "7"
    }
    if (!has7) "" else
    (if (hasMajSeven) {
      "M"
    } else "") + highestInt
  }

  def addedIntervals = {
    if (!has7) {
      (if (has9 && !no3rd) "add9" else "") +
      (if (has11 && !no3rd) "add11" else "") +
      (if (has13) "13" else "")
    } else ""
  }

  def determineInversion = {
    if (isDiminishedSeventh) {
      println("Is dim7")
      "root" //otherwise a diminished looks like 1st inversion
     } else {
    fretListToIntList(intervals.distinct).sorted map SEMI_TO_DEGREE match {
      case List(0, 3, 5) | List(0, 3, 5, 7) => "root" // case classes???
      case List(0, 3, 6) | List(0, 3, 5, 6) => "1st"
      case List(0, 4, 6) | List(0, 3, 4, 6) => "2nd"
      case List(0, 2, 4, 6) => "3rd"
//      case List(0, 3, 6) => "dim"
//      case List(0, 3, 6, 9) => "dim7"
//      case List(0, 4, 8) => "aug"

      case l => println(l); "root" // best guess
    }}
  }

  def respell = {
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
    println(this)
    new ChordNamer(fl, root)
  }

  def alterations = {
    (if (hasFlatFifth && !isDiminishedSeventh) {
      if (hasFifth) "♯11" else
      "♭5"} else "") +
    (if (hasFlat9) "♭9" else "")
  }

  def suspension = {
    if (no3rd) {
      if (has9) "sus2" else if (has11) "sus4" else ""

    } else ""
  }

  override def toString = {
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

  def fretListToIntList(fl: FretList) = fl.map{_.toList}.flatten.sorted

//  def intervals(fl: FretList, root: Int) = {
//    fl.zip(tuning.semitones).map{case (Some(n), t) => Some(norm(n+t-root)); case (None, _) => None}//.filterNot(_ == None).map(_.get)//.sorted
//  }

  // accepts list of intervals
  //def apply(chord: List[Int]) = new ChordNamer(chord.sorted)()

  //accepts fingering
  def apply(chord: String)(implicit tuning: Tuning): ChordNamer = {
    ChordNamer(Chord.unapply(chord)).respell
  }

  def apply(fl: FretList)(implicit tuning: Tuning) = {
    def getRoot(fl: FretList, tuning: List[Int]): Int = {
      if (fl.head.isDefined) norm(fl.head.get+tuning.head) else getRoot(fl.tail, tuning.tail)
    }
    val root = getRoot(fl, tuning.semitones)
    //val ints = intervals(fl, root)
    new ChordNamer(fl, root)//.respell //fretListToIntList(ints))
  }
}
