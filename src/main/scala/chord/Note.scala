package chord

/**
  * Created by ericolander on 4/30/17.
  */
case class Note(name: Char, accidental: Option[Accidental]) {

  "ABCDEFG".zip("2122122")

  val notes = "ABCDEFG".toCharArray

  def next(arr: Array[Char])(ch: Char): Char = arr((arr.indexOf(ch) + 1) % arr.length)

  def prev(arr: Array[Char])(ch: Char): Char = arr((arr.indexOf(ch) + arr.length - 1) % arr.length)

  override def toString: String = s"$name${accidental.getOrElse("")}"

  //def enharmonic: Note = Note(CircleOfFifths.enharmonic(this.toString))
  def enharmonic: Note = {
    def determineAccidental(note: Char, acc1: Option[Accidental], acc2: Option[Accidental]): Option[Accidental] = {
      if (note == 'E' || note == 'B') acc1 else acc2
    }

    def nextNote = next(notes)_
    def prevNote = prev(notes)_

    accidental.map { a =>
      a match {
        case Sharp() => Note(nextNote(name), determineAccidental(name, None, Some(Flat())))
        case Flat() => Note(prevNote(name), determineAccidental(prevNote(name), None, Some(Sharp())))
        case DoubleSharp() => Note(nextNote(name), determineAccidental(name, Some(Sharp()), None))
        case DoubleFlat() => Note(prevNote(name), determineAccidental(prevNote(name), Some(Flat()), None))
      }
    }.getOrElse(this)
  }
}

class Degree(val value: Int, val accidental: Option[Accidental]) {
  override def toString = s"${accidental.getOrElse("")}${value}"

  def adjust(note: Note): Note =
    accidental.map(_.adjust(note)).getOrElse(note)
}

sealed trait Accidental {
  def adjust(note: Note): Note
}

case class Sharp() extends Accidental {
  override def toString = "♯"

  override def adjust(note: Note): Note = {
    note.accidental.map {
      case Flat() => new Note(note.name, None)
      case DoubleFlat() => new Note(note.name, Some(Flat()))
      case Sharp() => new Note(note.name, Some(DoubleSharp()))
    }.getOrElse(new Note(note.name, Some(Sharp())))
  }
}

case class Flat() extends Accidental {
  override def toString = "♭"

  override def adjust(note: Note): Note = {
    note.accidental.map {
      case Sharp() => new Note(note.name, None)
      case Flat() => new Note(note.name, Some(DoubleFlat()))
      case DoubleSharp() => new Note(note.name, Some(Sharp()))
    }.getOrElse(new Note(note.name, Some(Flat())))
  }
}

case class DoubleSharp() extends Accidental {
  override def toString = DOUBLE_SHARP

  override def adjust(note: Note): Note = {
    note.accidental match {
      case None => new Note(note.name, Some(DoubleSharp()))
      case Some(Flat()) => new Note(note.name, Some(Sharp()))
      case Some(DoubleFlat()) => new Note(note.name, None)
    }
  }
}

case class DoubleFlat() extends Accidental {
  override def toString = DOUBLE_FLAT

  override def adjust(note: Note): Note = {
    note.accidental match {
      case None => new Note(note.name, Some(DoubleFlat()))
      case Some(Sharp()) => new Note(note.name, Some(Flat()))
      case Some(DoubleSharp()) => new Note(note.name, None)
    }
  }
}


object Note {
  def apply(n: String): Note = {
    new Note(n.head, Accidental(n.tail))
  }
}

object Accidental {
  def apply(s: String): Option[Accidental] = {
    if (s.length == 0) {
      None
    } else {
      Option(s match {
        case "♭" | "b" => Flat()
        case "♯" | "#" => Sharp()
        case "°" | DOUBLE_FLAT => DoubleFlat()
        case DOUBLE_SHARP => DoubleSharp()
        case _ => null
      })
    }
  }

//  def apply(s: String): Option[Accidental] = {
//    Option(s match {
//      case "♭" | "b" => Flat()
//      case "♯" | "#" => Sharp()
//      case "°" | DOUBLE_FLAT => DoubleFlat()
//      case DOUBLE_SHARP => DoubleSharp()
//      case _ => null
//    })
//  }
}

object Degree {
  def apply(s: String): Degree = {
    val accidental = if (s.head.isDigit) None else Accidental(s.takeWhile(c => !c.isDigit))
    val value = if (accidental.isDefined) s.tail.toInt else {
      if (s.head.isDigit) s.toInt else 0
    }
    new Degree(value, accidental)
  }
}

