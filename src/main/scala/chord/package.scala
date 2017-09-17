//package chord

import cats._

import scala.language.implicitConversions

package object chord {

  type FretList = List[Option[Int]]
  type DegreeList = List[Option[Degree]]
  type NoteList = List[Option[Note]]

  val DOUBLE_FLAT = "\uD834\uDD2B"
  val DOUBLE_SHARP = "\uD834\uDD2A"

  implicit val fretListShow: Show[FretList] = Show.show(_.map {_.getOrElse("x")}.mkString(" "))

  implicit val fretListOrder: Order[FretList] = new Order[FretList] {
    def compare(x: FretList, y: FretList): Int = {
      def helper(fl1: FretList, fl2: FretList): Int = {
      if (fl1.isEmpty || fl2.isEmpty) {
        0
      } else if (fl1.head.isEmpty) {
        helper(fl1.tail, fl2)
      } else if (fl2.head.isEmpty) {
        helper(fl1, fl2.tail)
      } else if (fl1.head.get == fl2.head.get) {
        helper(fl1.tail, fl2.tail)
      } else if (fl1.head.get < fl2.head.get) {
        -1
      } else {
        1
      }
    }
    helper(x, y)
    }
  }

  implicit val degreeListShow: Show[DegreeList] = Show.show(_.map {_.getOrElse("x")}.mkString(" "))

  implicit val noteListShow: Show[NoteList] = Show.show(_.map {_.getOrElse("x")}.mkString(" "))

  implicit class StringToFretList(val str: String) extends AnyVal {

    def fl: FretList = str.split(" ").map { case "x" => None; case n: String => Option(n.toInt) }.toList
  }

  private def mapAccidentals(note: Note, map: Map[Note, Int]): Int = {
    val noteVal = map(Note(note.name, Natural))
    if (note.accidental == Flat) {
      -1 + noteVal
    } else {
      1 + noteVal
    }
  }

  private def revMapAccidentals(note: Int, map: Map[Int, Note], preferSharps: Boolean = true): Note = {
    if (preferSharps) {
      Note(map(norm(note - 1)).name, Sharp)
    } else {
      Note(map(norm(note + 1)).name, Flat)}
  }

  val NOTE_MAP: Map[Note, Int] = {
    val m = Map(Note("E") -> 0, Note("F") -> 1, Note("G") -> 3, Note("A") -> 5, Note("B") -> 7,
      Note("C") -> 8, Note("D") -> 10)

      m.withDefault { r => mapAccidentals(r, m)
    }
  }

  val SEMI_TO_INT = Map(0 -> "R", 1 -> "♭9", 2 -> "9", 3 -> "♭3", 4 -> "3", 5 -> "11", 6 -> "♭5", 7 -> "5", 8 -> "♯5",
                        9 -> "13", 10 -> "♭7", 11 -> "7")

  implicit val tuning = Tuning.StandardTuning

  def retune(tuning: Tuning): Map[Note, Int] = {
    val newmap = NOTE_MAP.map { e => (e._1, norm(e._2 - NOTE_MAP(tuning.root))) }
    newmap.withDefault { r => mapAccidentals(r, newmap)}
  }

  def retune(root: Note): Map[Note, Int] = {
    val newmap = NOTE_MAP.map { e => (e._1, norm(e._2 - NOTE_MAP(root))) }
    newmap.withDefault { r => mapAccidentals(r, newmap)}
  }

  def reverseNoteMap(root: Note, preferSharps: Boolean = true): Map[Int, Note] = {
    val map = retune(root).map{case(k, v) => v -> k }
    map.withDefault(n=>revMapAccidentals(n, map, preferSharps))
  }

  def hasAccidental(n: String): Boolean = "♭b♯#°".toSet(n.head)

  def norm(x: Int, b: Int = 12): Int = (x + b) % b

  //converts a space delimited string into a list, eliminating extraneous whitespace
  def delimitedToList(s: String): List[String] = s.trim.split(" ").toList.filter{!_.isEmpty}

  def diff(a: FretList, b: FretList): Int = {
    val shift = math.max(math.abs(a.max.get - b.filter{v => v.isDefined && v.get > 0}.min.get), math.abs(b.max.get - a.filter{v => v.isDefined && v.get > 0}.min.get))
    a.zip(b).map {
      case (None, None) => 0
      case (Some(0), Some(0)) => 0
      case (None, Some(0)) => 0
      case (Some(0), None) => 0
      case (None, Some(_)) => 1
      case (Some(_), None) => 1
      case (Some(_), Some(0)) => 1
      case (Some(0), Some(_)) => 1
      case e =>
        math.abs(e._1.get - e._2.get)
    }.sum + shift
  }
}
