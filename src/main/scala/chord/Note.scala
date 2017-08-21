package chord

/**
  * Created by ericolander on 4/30/17.
  */
case class Note(name: Char, accidental: Accidental) {

  private val notes = "ABCDEFG".toCharArray

  def next(arr: Array[Char])(ch: Char): Char = arr((arr.indexOf(ch) + 1) % arr.length)

  def prev(arr: Array[Char])(ch: Char): Char = arr((arr.indexOf(ch) + arr.length - 1) % arr.length)

  override def toString: String = s"$name$accidental"

  val isValid = true

  def enharmonic: Note = {
    def determineAccidental(note: Char, acc1: Accidental, acc2: Accidental): Accidental = {
      if (note == 'E' || note == 'B') acc1 else acc2
    }

    def nextNote = next(notes)_
    def prevNote = prev(notes)_

    accidental match {
        case Sharp() => Note(nextNote(name), determineAccidental(name, Natural(), Flat()))
        case Flat() => Note(prevNote(name), determineAccidental(prevNote(name), Natural(), Sharp()))
        case DoubleSharp() => Note(nextNote(name), determineAccidental(name, Sharp(), Natural()))
        case DoubleFlat() => Note(prevNote(name), determineAccidental(prevNote(name), Flat(), Natural()))
        case Natural() => this
      }
  }
}

case class Degree(value: Int, accidental: Option[Accidental]) {
  override def toString: String = s"${accidental.getOrElse("")}${if (value == 0) "R" else value}"

  def adjust(note: Note): Note =
    accidental.map(_.adjust(note)).getOrElse(note)

  def adjust(degree: Degree): Degree =
    accidental.map { a => Degree(degree.value, a.adjust(degree.accidental)) }.getOrElse(degree)
}

sealed trait Accidental {
  def adjust(note: Note): Note

  def adjust(a: Option[Accidental]): Option[Accidental] = a
}

case class Natural() extends Accidental {
  override def toString: String = ""

  override def adjust(note: Note): Note = {
    note
  }
}

case class Sharp() extends Accidental {
  override def toString: String = "♯"

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Flat() => new Note(note.name, Natural())
      case DoubleFlat() => new Note(note.name, Flat())
      case Sharp() => new Note(note.name, DoubleSharp())
      case Natural() => new Note(note.name, Sharp())
    }
  }

  override def adjust(a: Option[Accidental]): Option[Accidental] =
    a.map {
      case DoubleSharp() => Sharp()
      case Sharp() => Natural()
    }
}

case class Flat() extends Accidental {
  override def toString: String = "♭"

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Sharp() => new Note(note.name, Natural())
      case Flat() => new Note(note.name, DoubleFlat())
      case DoubleSharp() => new Note(note.name, Sharp())
      case Natural() => new Note(note.name, Flat())
    }
  }

  override def adjust(a: Option[Accidental]): Option[Accidental] =
    a.map {
      case DoubleFlat() => Flat()
      case Flat() => Natural()
    }
}

case class DoubleSharp() extends Accidental {
  override def toString: String = DOUBLE_SHARP

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Natural() => new Note(note.name, DoubleSharp())
      case Flat() => new Note(note.name, Sharp())
      case DoubleFlat() => new Note(note.name, Natural())
    }
  }
}

case class DoubleFlat() extends Accidental {
  override def toString: String = DOUBLE_FLAT

  override def adjust(note: Note): Note = {
    note.accidental match {
      case Natural() => new Note(note.name, DoubleFlat())
      case Sharp() => new Note(note.name, Flat())
      case DoubleSharp() => new Note(note.name, Natural())
    }
  }
}

object Note {
  def apply(n: String): Note = {
    new Note(n.head, Accidental(n.tail))
  }

  def notes(str: String): Seq[Note] = str.split(",").map{ n => apply(n.trim)}.toSeq
}

object InvalidNote extends Note(' ', Natural()) {
  override val isValid = false
}

object Accidental {
  def apply(s: String): Accidental = {
    if (s.length == 0) {
      Natural()
    } else {
      s match {
        case "♭" | "b" => Flat()
        case "♯" | "#" => Sharp()
        case "°" | DOUBLE_FLAT => DoubleFlat()
        case DOUBLE_SHARP => DoubleSharp()
        case _ => null
      }
    }
  }
}

object Degree {
  def apply(s: String): Degree = {
    val accidental = if (s.head.isDigit) None else Option(Accidental(s.takeWhile(c => !c.isDigit)))
    val value = if (accidental.isDefined) s.tail.toInt else {
      if (s.head.isDigit) s.toInt else 0
    }
    new Degree(value, accidental)
  }
}

