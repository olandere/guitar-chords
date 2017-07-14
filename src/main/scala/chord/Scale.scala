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
  val notes: Map[Int, Note] = {
    val mapping = reverseNoteMap(root)
    if (root.accidental != Sharp() && intervals.map(mapping).toSet.size != 7) {
      reverseNoteMap(root, preferSharps = false)
    } else {
      mapping
    }
  }


  def relatedScale: Scale
}

case class MajorScale(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 5, 7, 9, 11)//List(2,2,1,2,2,2,1)

  def relatedScale: Scale = this

  def relativeMinor: Scale = Aeolian(notes(9))
}

case class HarmonicMinor(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 8, 11)

  def relatedScale: Scale = this
}

case class Dorian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 9, 10)//List(2,1,2,2,2,1,2)

  def relatedScale: Scale = MajorScale(notes(10))
}

case class Phrygian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 5, 7, 8, 10)

  def relatedScale: Scale = MajorScale(notes(8))
}

case class PhrygianDominant(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 10)

  def relatedScale: Scale = HarmonicMinor(notes(5))
}

case class DoubleHarmonic(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 11)

  def relatedScale: Scale = this
}

case class Lydian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 6, 7, 9, 11)

  def relatedScale: Scale = MajorScale(notes(7))
}

case class LydianDominant(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 6, 7, 9, 10)

  def relatedScale: Scale = MajorScale(notes(7))
}

case class Mixolydian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 5, 7, 9, 10)

  def relatedScale: Scale = MajorScale(notes(5))
}

case class Aeolian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 8, 10)

  def relatedScale: Scale = MajorScale(notes(3))
}

case class Locrian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 5, 6, 8, 10)

  def relatedScale: Scale = MajorScale(notes(1))
}

case class SuperLocrian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 4, 6, 8, 10)

  def relatedScale: Scale = MajorScale(notes(1))
}

case class MinorPent(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 3, 5, 7, 10)

  def relatedScale: Scale = MajorScale(notes(3))
}

case class MajorPent(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 7, 9)

  def relatedScale: Scale = MajorScale(notes(0))
}

object Scale {

  def allScales(root: Note): List[Scale] = {
    List(MajorScale(root), Dorian(root), Phrygian(root), PhrygianDominant(root), Lydian(root), LydianDominant(root),
         Mixolydian(root), Aeolian(root), Locrian(root), SuperLocrian(root), MinorPent(root), MajorPent(root))
  }
}
