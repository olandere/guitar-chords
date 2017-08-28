package chord

/**
 * Created by eolander on 3/22/15.
 */
sealed trait Scale {

  def root: Note

  def intervals: List[Int]

  def semitones: List[Int] = intervals.zip(intervals.tail :+ intervals.head)
    .map { case (a, b) => ((b - a) + 12) % 12 }

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

  override def toString: String = s"$root Major"
}

case class HarmonicMinor(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 8, 11)

  def relatedScale: Scale = this

  override def toString: String = s"$root Harmonic Minor"
}

case class MelodicMinor(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 9, 11)

  def relatedScale: Scale = this

  override def toString: String = s"$root Melodic Minor"
}

case class Dorian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(1)//List(2,1,2,2,2,1,2)

  def relatedScale: Scale = MajorScale(noteMap(10))

  override def toString: String = s"$root Dorian"
}

case class Phrygian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(2)

  def relatedScale: Scale = MajorScale(noteMap(8))

  override def toString: String = s"$root Phrygian"
}

case class PhrygianDominant(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 10)

  def relatedScale: Scale = HarmonicMinor(noteMap(5))

  override def toString: String = s"$root Phrygian Dominant"
}

case class DoubleHarmonic(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 4, 5, 7, 8, 11)

  def relatedScale: Scale = this

  override def toString: String = s"$root Double Harmonic"
}

case class Lydian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(3)

  def relatedScale: Scale = MajorScale(noteMap(7))

  override def toString: String = s"$root Lydian"
}

case class LydianDominant(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 6, 7, 9, 10)

  def relatedScale: Scale = MajorScale(noteMap(7))

  override def toString: String = s"$root Lydian Dominant"
}

case class Mixolydian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(4)

  def relatedScale: Scale = MajorScale(noteMap(5))

  override def toString: String = s"$root Mixolydian"
}

case class Aeolian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(5)

  def relatedScale: Scale = MajorScale(noteMap(3))

  override def toString: String = s"$root Aeolian"
}

case class Locrian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(6)

  def relatedScale: Scale = MajorScale(noteMap(1))

  override def toString: String = s"$root Locrian"
}

case class SuperLocrian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 4, 6, 8, 10)

  def relatedScale: Scale = MajorScale(noteMap(1))

  override def toString: String = s"$root Super Locrian"
}

case class MinorPent(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 3, 5, 7, 10)

  def relatedScale: Scale = MajorScale(noteMap(3))

  override def toString: String = s"$root Minor Pentatonic"
}

case class MajorPent(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 7, 9)

  def relatedScale: Scale = MajorScale(noteMap(0))

  override def toString: String = s"$root Major Pentatonic"
}

case class ScaleByDegrees(root: Note, degrees: List[Degree]) extends Scale {
  override def intervals: List[Int] = List(0) ++ degrees.map(_.semitone)

  def relatedScale: Scale = this

  override def toString: String = s"$root ${degrees.mkString(" ")}"
}

object Scale {

  def allScales(root: Note): List[Scale] = {
    List(MajorScale(root), Dorian(root), Phrygian(root), PhrygianDominant(root), Lydian(root), LydianDominant(root),
         Mixolydian(root), Aeolian(root), Locrian(root), SuperLocrian(root), MinorPent(root), MajorPent(root))
  }

  def apply(root: Note, scaleName: String): Scale = {
    scaleName match {
      case "Major" => MajorScale(root)
      case "Harmonic Minor" => HarmonicMinor(root)
      case "Melodic Minor" => MelodicMinor(root)
      case "Dorian" => Dorian(root)
      case "Phrygian" => Phrygian(root)
      case "Lydian" => Lydian(root)
      case "Mixolydian" => Mixolydian(root)
      case "Aeolian" => Aeolian(root)
      case "Locrian" => Locrian(root)
      case "Phrygian Dominant" => PhrygianDominant(root)
      case "Double Harmonic" => DoubleHarmonic(root)
      case "Lydian Dominant" => LydianDominant(root)
      case "Super Locrian" => SuperLocrian(root)
      case "Minor Pentatonic" => MinorPent(root)
      case "Major Pentatonic" => MajorPent(root)
      case _ => {
        val degrees = DegreeParser(scaleName)
        if (degrees.nonEmpty)
          ScaleByDegrees(root, degrees)
        else MajorScale(root)
      }
    }
  }
}
