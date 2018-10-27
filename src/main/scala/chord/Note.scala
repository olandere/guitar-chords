package chord

/**
  * Created by ericolander on 4/30/17.
  */
case class Note(name: Char, accidental: Accidental) {

  private val notes = "ABCDEFG".toCharArray

  //require(notes.toSet.contains(name), s"invalid name: $name")

  def next(arr: Array[Char])(ch: Char): Char = arr((arr.indexOf(ch) + 1) % arr.length)

  def prev(arr: Array[Char])(ch: Char): Char = arr((arr.indexOf(ch) + arr.length - 1) % arr.length)

  def next(semitones: Int): Note = {
    require(semitones > 0 && semitones < 4)
    val note = next(notes)(name)
    semitones match {
      case 1 =>
        if (name == 'E' || name == 'B') {
          Note(note, this.accidental)
        } else {
          Note(note, this.accidental.lower)
        }
      case 2 =>
        if (name == 'E' || name == 'B') {
          Note(note, this.accidental.raise)
        } else {
          Note(note, this.accidental)
        }
      case 3 =>
        if (name == 'E' || name == 'B') {
          Note(note, this.accidental.raise.raise)
        } else {
          Note(note, this.accidental.raise)
        }
    }
  }

  override def toString: String = s"$name$accidental"

  def enhEquals(that: Note): Boolean = {
    this.equals(that) || this.enharmonic.equals(that) || this.equals(that.enharmonic)
  }

  def interval(that: Note): Interval = {
    Major(this).semitone(that) match {
      case(d, n) if d == 1 || d == 4 || d == 5 => n match {
        case -3 => Interval(s"ddd$d")
        case -2 => Interval(s"dd$d")
        case -1 => Interval(s"d$d")
        case 0 => Interval(s"P$d")
        case 1 => Interval(s"A$d")
        case 2 => Interval(s"AA$d")
        case 3 => Interval(s"AAA$d")
      }
      case (d, n) => n match {
        case -3 => Interval(s"dd$d")
        case -2 => Interval(s"d$d")
        case -1 => Interval(s"m$d")
        case 0 => Interval(s"M$d")
        case 1 => Interval(s"A$d")
        case 2 => Interval(s"AA$d")
      }
    }
  }

  def raise(interval: Interval): Note = {
    val notes = Major(this).notes
    val note = notes((interval.number - 1) % notes.length)
    interval.quality match {
      case Minor =>
        Note(note.name, note.accidental.lower)
      case Diminished =>
        if (Set(1, 4, 5, 8).contains(interval.number)) {
          Note(note.name, note.accidental.lower)
        } else {
          Note(note.name, note.accidental.lower.lower)
        }
      case DoublyDiminished =>
        if (Set(1, 4, 5, 8).contains(interval.number)) {
          Note(note.name, note.accidental.lower.lower)
        } else {
          Note(note.name, note.accidental.lower.lower.lower)
        }
      case TripleDiminished =>
        if (Set(1, 4, 5, 8).contains(interval.number)) {
          Note(note.name, note.accidental.lower.lower.lower)
        } else {
          Note(note.name, note.accidental.lower.lower.lower.lower)
        }
      case Augmented =>
        Note(note.name, note.accidental.raise)
      case DoublyAugmented =>
        Note(note.name, note.accidental.raise.raise)
      case TripleAugmented =>
        Note(note.name, note.accidental.raise.raise.raise)

      case _ => note
    }
  }

  def lower(interval: Interval): Note = raise(interval.invert)

//  override def equals(obj: scala.Any): Boolean = obj match {
//    case n:Note => (name == n.name && accidental == n.accidental) || this.
//    case _ => false
//  }

  val isValid = true

  def enharmonic: Note = {
    def determineAccidental(note: Char, acc1: Accidental, acc2: Accidental): Accidental = {
      if (note == 'E' || note == 'B') acc1 else acc2
    }

    def nextNote = next(notes)_
    def prevNote = prev(notes)_

    accidental match {
        case Sharp => Note(nextNote(name), determineAccidental(name, Natural, Flat))
        case Flat => Note(prevNote(name), determineAccidental(prevNote(name), Natural, Sharp))
        case DoubleSharp => Note(nextNote(name), determineAccidental(name, Sharp, Natural))
        case DoubleFlat => Note(prevNote(name), determineAccidental(prevNote(name), Flat, Natural))
        case Natural => this
      }
  }
}

case class Degree(value: Int, accidental: Accidental) {
  override def toString: String = s"${accidental}${if (value == 0) "R" else value}"

  def isValid: Boolean = true

  def adjust(note: Note): Note =
    accidental.adjust(note)

  def adjust(degree: Degree): Degree =
    Degree(degree.value, accidental.adjust(degree.accidental))

  def semitone: Int = {
    val s = Major(Note("C")).intervals(
      if (value > 0) {
        if (value > 7) {
          value - 8
        } else {
          value - 1
        }
      } else {
        value
      })
    accidental match {
      case Sharp => s + 1
      case Flat => s - 1
      case _ => s
    }
  }

  def interval(that: Degree): Interval = {
    if (this == that) Interval("P1") else {
      val n = norm(this.semitone - that.semitone)
      val int = ((n + 1) / 2) + 1
      val q = if (Set(1, 3, 8, 10)(n)) "m" else if (Set(5, 7)(n)) "P" else "M"
      Interval(s"$q$int")
    }
  }

  def toInterval: Interval = interval(Degree("R"))

  def next(semitones: Int): Degree = {
    require(semitones > 0 && semitones < 4)
    val deg = if (value == 0) 2 else value + 1
    semitones match {
      case 1 =>
        if (value == 3 || value == 7) {
          Degree(deg, this.accidental)
        } else {
          Degree(deg, this.accidental.lower)
        }
      case 2 =>
        if (value == 3 || value == 7) {
          Degree(deg, this.accidental.raise)
        } else {
          Degree(deg, this.accidental)
        }
      case 3 =>
        if (value == 3 || value == 7) {
          Degree(deg, this.accidental.raise.raise)
        } else {
          Degree(deg, this.accidental.raise)
        }
    }
  }
}

sealed trait Accidental extends Ordered[Accidental] {

  val order: Int
  def adjust(note: Note): Note

  def adjust(a: Accidental): Accidental = a

  override def compare(that: Accidental): Int = this.order - that.order

  val raise: Accidental

  val lower: Accidental
}

case object Natural extends Accidental {
  override def toString: String = ""

  override val order: Int = 0

  override def adjust(note: Note): Note = {
    note
  }

  override val lower: Accidental = Flat
  override val raise: Accidental = Sharp
}

case object Sharp extends Accidental {
  override def toString: String = "♯"

  override val order: Int = 1

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Flat => new Note(note.name, Natural)
      case DoubleFlat => new Note(note.name, Flat)
      case Sharp => new Note(note.name, DoubleSharp)
      case Natural => new Note(note.name, Sharp)
    }
  }

  override def adjust(a: Accidental): Accidental =
    a match {
      case DoubleSharp => Sharp
      case Sharp => Natural
      case Natural => Natural
    }

  override val lower: Accidental = Natural
  override val raise: Accidental = DoubleSharp
}

case object Flat extends Accidental {
  override def toString: String = "♭"

  override val order: Int = -1

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Sharp => new Note(note.name, Natural)
      case Flat => new Note(note.name, DoubleFlat)
      case DoubleSharp => new Note(note.name, Sharp)
      case Natural => new Note(note.name, Flat)
    }
  }

  override def adjust(a: Accidental): Accidental =
    a match {
      case DoubleFlat => Flat
      case Flat => Natural
      case Natural => Sharp
      case Dim => Flat
    }

  override val lower: Accidental = DoubleFlat
  override val raise: Accidental = Natural
}

case object DoubleSharp extends Accidental {
  override def toString: String = DOUBLE_SHARP

  override val order: Int = 2

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Natural => new Note(note.name, DoubleSharp)
      case Flat => new Note(note.name, Sharp)
      case DoubleFlat => new Note(note.name, Natural)
    }
  }

  override val lower: Accidental = Sharp
  override val raise: Accidental = Sharp
}

case object DoubleFlat extends Accidental {
  override def toString: String = DOUBLE_FLAT

  override val order: Int = -2

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Natural => new Note(note.name, DoubleFlat)
      case Sharp => new Note(note.name, Flat)
      case DoubleSharp => new Note(note.name, Natural)
    }
  }

  override val raise: Accidental = Flat
  override val lower: Accidental = Flat
}

case object Dim extends Accidental {
  override def toString: String = "°"

  override val order: Int = -2

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Natural => new Note(note.name, DoubleFlat)
      case Sharp => new Note(note.name, Flat)
      case DoubleSharp => new Note(note.name, Natural)
    }
  }

  override val raise: Accidental = Flat
  override val lower: Accidental = Flat
}

object Note {
  def apply(n: String): Note = {
    new Note(n.head.toTitleCase, Accidental(n.tail))
  }

  def notes(str: String): Seq[Note] = str.split(",").map{ n => apply(n.trim)}.toSeq
}

object InvalidNote extends Note(' ', Natural) {
  override val isValid = false
}

object Accidental {
  def apply(s: String): Accidental = {
    if (s.length == 0) {
      Natural
    } else {
      s match {
        case "♭" | "b" => Flat
        case "♯" | "#" => Sharp
        case "°" => Dim
        case DOUBLE_FLAT => DoubleFlat
        case DOUBLE_SHARP => DoubleSharp
        case _ => Natural
      }
    }
  }
}

object Degree {
  def apply(s: String): Degree = {
    DegreeParser(s).head
  }

  def apply(value: Int, accidental: Accidental): Degree = new Degree(value, accidental)
}

object InvalidDegree extends Degree(0, Natural) {
  override val isValid = false
}

