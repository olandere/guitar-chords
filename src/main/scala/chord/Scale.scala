package chord

/**
 * Created by eolander on 3/22/15.
 */
sealed trait Scale {

  def root: Note

  def intervals: List[Int]

  def containsChord(chord: Chord): Boolean = {
    val semitones = chord.semitones.map { s => norm(s + retune(root)(chord.root)) }
    semitones.intersect(intervals) == semitones
  }

  //val preferSharps = Set("G", "D", "A", "E", "B").contains(root) || root.contains("♯")
  val noteMap: Map[Int, Note] = {
    val mapping = reverseNoteMap(root, !CircleOfFifths.flatKeys.contains(root))
    if (intervals.map(mapping).toSet.size != 7) {
      reverseNoteMap(root, preferSharps = false)
    } else {
      mapping
    }
  }

  def notes: Seq[Note] = for {i <- intervals} yield noteMap(i)

  def relatedScale: Scale

  def rotateLeft[A](l: List[A], n: Int = 1):List[A] = l.drop(n) ++ l.take(n)

  def major = List(2,2,1,2,2,2,1)
  
  def genIntervals(n: Int): List[Int] = rotateLeft(major, n).scanLeft(0)(_+_).take(7)
}

case class MajorScale(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(0)

  def relatedScale: Scale = this

  def relativeMinor: Scale = Aeolian(noteMap(9))
}

case class HarmonicMinor(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 8, 11)

  def relatedScale: Scale = this
}

case class Dorian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(1)//List(2,1,2,2,2,1,2)

  def relatedScale: Scale = MajorScale(noteMap(10))
}

case class Phrygian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(2)

  def relatedScale: Scale = MajorScale(noteMap(8))
}

case class PhrygianDominant(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 10)

  def relatedScale: Scale = HarmonicMinor(noteMap(5))
}

case class DoubleHarmonic(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 11)

  def relatedScale: Scale = this
}

case class Lydian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(3)

  def relatedScale: Scale = MajorScale(noteMap(7))
}

case class LydianDominant(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 6, 7, 9, 10)

  def relatedScale: Scale = MajorScale(noteMap(7))
}

case class Mixolydian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(4)

  def relatedScale: Scale = MajorScale(noteMap(5))
}

case class Aeolian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(5)

  def relatedScale: Scale = MajorScale(noteMap(3))
}

case class Locrian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(6)

  def relatedScale: Scale = MajorScale(noteMap(1))
}

case class SuperLocrian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 4, 6, 8, 10)

  def relatedScale: Scale = MajorScale(noteMap(1))
}

case class MinorPent(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 3, 5, 7, 10)

  def relatedScale: Scale = MajorScale(noteMap(3))
}

case class MajorPent(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 7, 9)

  def relatedScale: Scale = MajorScale(noteMap(0))
}

object Scale {

  def allScales(root: Note): List[Scale] = {
    List(MajorScale(root), Dorian(root), Phrygian(root), PhrygianDominant(root), Lydian(root), LydianDominant(root),
         Mixolydian(root), Aeolian(root), Locrian(root), SuperLocrian(root), MinorPent(root), MajorPent(root))
  }
}
