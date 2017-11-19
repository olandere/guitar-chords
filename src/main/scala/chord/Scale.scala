package chord

/**
 * Created by eolander on 3/22/15.
 */
sealed trait Scale {

  import Scale._

  val name: String = getName(this.getClass.toString.drop(12))

  def root: Note

  def intervals: List[Int]

  def semitones: List[Int] = intervals.zip(intervals.tail :+ intervals.head)
    .map { case (a, b) => mod12(b - a) }

  def containsChord(chord: Chord): Boolean = {
    val semitones = chord.semitones.map { s => norm(s + retune(root)(chord.root)) }
    semitones.intersect(intervals) == semitones
  }

  //val preferSharps = Set("G", "D", "A", "E", "B").contains(root) || root.contains("♯")
  val noteMap: Map[Int, Note] = {
    val mapping = reverseNoteMap(root, isSharpKey)
    if (intervals.map(mapping).toSet.size != 7) {
      reverseNoteMap(root, preferSharps = false)
    } else {
      mapping
    }
  }

  //def notes: Seq[Note] = for {i <- intervals} yield noteMap(i)

  def notes: Seq[Note] = semitones.take(semitones.length - 1).scanLeft(root)((n, s) => n.next(s))

  def isSharpKey: Boolean = notes.exists(n => n.accidental == Sharp)

  def degrees: Seq[Degree] = semitones.take(semitones.length - 1).scanLeft(Degree("R"))((d, s) => d.next(s))

  def relatedScale: Scale

  private def rotateLeft[A](l: List[A], n: Int = 1):List[A] = l.drop(n) ++ l.take(n)

  def major = List(2,2,1,2,2,2,1)
  
  def genIntervals(n: Int): List[Int] = rotateLeft(major, n).scanLeft(0)(_+_).take(7)

  override def toString: String = s"$root $name"

  def nearestNote(note: Note) = {
    notes.find(n => n.name == note.name)
  }

  def semitone(note: Note): (Int, Int) = {
    if (notes.contains(note)) (notes.indexOf(note)+1, 0)
    else {
      val nearest = nearestNote(note)
      nearest.map{n => (notes.indexOf(n)+1, note.accidental.order - n.accidental.order)}
        .getOrElse((0, 0))
    }
  }

  def noteFromDegree(degree: Degree): Note = {
    val note = if (degree.value == 0) notes.head
    else if (degree.value > 7) notes(degree.value - 8)
    else
      notes(degree.value - 1)
    degree.accidental.adjust(note)
  }
}

case class Major(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(0)

  def relatedScale: Scale = this

  def relativeMinor: Scale = Aeolian(noteMap(9))
}

case class HarmonicMinor(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 8, 11)

  def relatedScale: Scale = this
}

case class MelodicMinor(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 3, 5, 7, 9, 11)

  def relatedScale: Scale = this
}

case class Dorian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(1)//List(2,1,2,2,2,1,2)

  def relatedScale: Scale = Major(noteMap(10))
}

case class Phrygian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(2)

  def relatedScale: Scale = Major(noteMap(8))
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

  def relatedScale: Scale = Major(noteMap(7))
}

case class LydianDominant(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 6, 7, 9, 10)

  def relatedScale: Scale = Major(noteMap(7))
}

case class Mixolydian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(4)

  def relatedScale: Scale = Major(noteMap(5))
}

case class Aeolian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(5)

  def relatedScale: Scale = Major(noteMap(3))
}

case class Locrian(root: Note) extends Scale {

  def intervals: List[Int] = genIntervals(6)

  def relatedScale: Scale = Major(noteMap(1))
}

case class SuperLocrian(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 1, 3, 4, 6, 8, 10)

  def relatedScale: Scale = Major(noteMap(1))
}

case class MinorPentatonic(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 3, 5, 7, 10)

  def relatedScale: Scale = Major(noteMap(3))
}

case class MajorPentatonic(root: Note) extends Scale {

  def intervals: List[Int] = List(0, 2, 4, 7, 9)

  def relatedScale: Scale = Major(noteMap(0))
}

case class ScaleByDegrees(root: Note, deg: List[Degree]) extends Scale {
  override def intervals: List[Int] = List(0) ++ deg.map(_.semitone)

  def relatedScale: Scale = this

  override def toString: String = s"$root ${deg.mkString(" ")}"
}

object Scale {

  private val nameSplitter = """(?<=.)(?=(\p{Upper}))""".r

  def getName(n: String): String = nameSplitter.split(n).mkString(" ")

  private val allScaleClasses = {
    import scala.reflect.runtime.{universe => ru}

    val tpe = ru.typeOf[Scale]
    val clazz = tpe.typeSymbol.asClass
    clazz.knownDirectSubclasses
  }

  val supportedScales: List[String] = {
    allScaleClasses
      .filterNot(_.fullName.contains("ScaleByDegrees"))
      .map(c => getName(c.fullName.split("\\.").tail.head)).toList
  }

  def allScales(root: Note): List[Scale] = {
    import scala.reflect.runtime.{universe => ru}

    val m = ru.runtimeMirror(this.getClass.getClassLoader)

    allScaleClasses.filterNot(_.fullName.contains("ScaleByDegrees")).map{c =>
      val cm = m.reflectClass(c.asClass)
      val ctor = c.asClass.primaryConstructor.asMethod
      val ctorm = cm.reflectConstructor(ctor)
      ctorm(root).asInstanceOf[Scale]
    }.toList
  }

  def apply(root: Note, scaleName: String): Scale = {
    scaleName match {
      case "Major" => Major(root)
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
      case "Minor Pentatonic" => MinorPentatonic(root)
      case "Major Pentatonic" => MajorPentatonic(root)
      case _ =>
        val degrees = DegreeParser(scaleName)
        if (degrees.nonEmpty)
          ScaleByDegrees(root, degrees)
        else Major(root)
    }
  }
}
