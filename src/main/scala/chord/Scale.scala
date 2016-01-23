package chord

/**
 * Created by eolander on 3/22/15.
 */
sealed trait Scale {

  def root: String

  def intervals: List[Int]

  def containsChord(chord: Chord) = {
    val semitones = chord.semitones.map { s => norm(s + retune(root)(chord.root)) }
    semitones.intersect(intervals) == semitones
  }

  //val preferSharps = Set("G", "D", "A", "E", "B").contains(root) || root.contains("♯")
  val notes = {
    val map = reverseNoteMap(root)
    if (!root.contains("♯") && intervals.map(map).map(_.head).toSet.size != 7) {
      reverseNoteMap(root, preferSharps = false)
    } else {
      map
    }
  }

  def relatedScale: Scale
}

case class MajorScale(root: String) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 5, 7, 9, 11)//List(2,2,1,2,2,2,1)

  def relatedScale = this
}

case class HarmonicMinor(root: String) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 8, 11)

  def relatedScale = this
}

case class Dorian(root: String) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 9, 10)//List(2,1,2,2,2,1,2)

  def relatedScale = MajorScale(notes(10))
}

case class Phrygian(root: String) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 5, 7, 8, 10)

  def relatedScale = MajorScale(notes(8))
}

case class PhrygianDominant(root: String) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 10)

  def relatedScale = HarmonicMinor(notes(5))
}

case class DoubleHarmonic(root: String) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 11)

  def relatedScale = this
}

case class Lydian(root: String) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 6, 7, 9, 11)

  def relatedScale = MajorScale(notes(7))
}

case class LydianDominant(root: String) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 6, 7, 9, 10)

  def relatedScale = MajorScale(notes(7))
}

case class Mixolydian(root: String) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 5, 7, 9, 10)

  def relatedScale = MajorScale(notes(5))
}

case class Aeolian(root: String) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 8, 10)

  def relatedScale = MajorScale(notes(3))
}

case class Locrian(root: String) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 5, 6, 8, 10)

  def relatedScale = MajorScale(notes(1))
}

case class SuperLocrian(root: String) extends Scale {

  def intervals = List(0, 1, 3, 4, 6, 8, 10)

  def relatedScale = MajorScale(notes(1))
}

case class MinorPent(root: String) extends Scale {

  def intervals = List(0, 3, 5, 7, 10)

  def relatedScale = MajorScale(notes(3))
}

case class MajorPent(root: String) extends Scale {

  def intervals = List(0, 2, 4, 7, 9)

  def relatedScale = MajorScale(notes(0))
}

object Scale {

  def allScales(root: String) = {
    List(MajorScale(root), Dorian(root), Phrygian(root), PhrygianDominant(root), Lydian(root), LydianDominant(root),
         Mixolydian(root), Aeolian(root), Locrian(root), SuperLocrian(root), MinorPent(root), MajorPent(root))
  }
}
