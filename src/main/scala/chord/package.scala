//package chord

import scalaz.Ordering.{GT, LT, EQ}
import scalaz._
import scala.language.implicitConversions

package object chord {
  type FretList = List[Option[Int]]
  type DegreeList = List[Option[String]]

  implicit val fretListShow: Show[FretList] = Show.shows(_.map {_.getOrElse("x")}.mkString(" "))

  implicit val fretListOrder: Order[FretList] = Order.order((a, b) => {
    def helper(fl1: FretList, fl2: FretList): Ordering = {
      if (fl1.isEmpty || fl2.isEmpty) {
        EQ
      } else if (!fl1.head.isDefined) {
        helper(fl1.tail, fl2)
      } else if (!fl2.head.isDefined) {
        helper(fl1, fl2.tail)
      } else if (fl1.head.get == fl2.head.get) {
        helper(fl1.tail, fl2.tail)
      } else if (fl1.head.get < fl2.head.get) {
        LT
      } else {
        GT
      }
    }
    helper(a, b)
  })

  implicit val degreeListShow: Show[DegreeList] = Show.shows(_.map {_.getOrElse("x")}.mkString(" "))

  private def mapAccidentals(note: String, map: Map[String, Int]) = {
    if (note.endsWith("b") || note.endsWith("♭")) {
      -1 + map(note(0).toString)
    } else {
      1 + map(note(0).toString)
    }
  }

  private def revMapAccidentals(note: Int, map: Map[Int, String], preferSharps: Boolean = true): String = {
    if (preferSharps) {
      map(norm(note - 1)) + "♯"
    } else {map(norm(note + 1)) + "♭"}
  }

  val NOTE_MAP: Map[String, Int] =
    Map("E" -> 0, "F" -> 1, "G" -> 3, "A" -> 5, "B" -> 7, "C" -> 8, "D" -> 10).withDefault { r =>
      mapAccidentals(r, NOTE_MAP)
    }

  val SEMI_TO_INT = Map(0 -> "R", 1 -> "♭9", 2 -> "9", 3 -> "♭3", 4 -> "3", 5 -> "11", 6 -> "♭5", 7 -> "5", 8 -> "♯5",
                        9 -> "13", 10 -> "♭7", 11 -> "7")

  implicit val tuning = Tuning.StandardTuning

  def retune(tuning: Tuning): Map[String, Int] = {
    val newmap = NOTE_MAP.map { e => (e._1, norm(e._2 - NOTE_MAP(tuning.root))) }
    newmap.withDefault { r => mapAccidentals(r, newmap)}
  }

  def retune(root: String): Map[String, Int] = {
    val newmap = NOTE_MAP.map { e => (e._1, norm(e._2 - NOTE_MAP(root))) }
    newmap.withDefault { r => mapAccidentals(r, newmap)}
  }

  def reverseNoteMap(root: String, preferSharps: Boolean = true): Map[Int, String] = {
    val map = retune(root).map{case(k, v) => v -> k }
    map.withDefault(n=>revMapAccidentals(n, map, preferSharps))
  }

  def norm(x: Int): Int = (x + 12) % 12

  //converts a space delimited string into a list, eliminating extraneous whitespace
  def delimitedToList(s: String): List[String] = s.trim.split(" ").toList.filter{!_.isEmpty}

  def diff(a: FretList, b: FretList): Int = {
    val shift = math.max(math.abs(a.max.get - b.filter{v => v.isDefined && v.get > 0}.min.get), math.abs(b.max.get - a.filter{v => v.isDefined && v.get > 0}.min.get))
    a.zip(b).map { e =>
      e match {
        case (None, None) => 0
        case (Some(0), Some(0)) => 0
        case (None, Some(0)) => 0
        case (Some(0), None) => 0
        case (None, Some(_)) => 1
        case (Some(_), None) => 1
        case (Some(_), Some(0)) => 1
        case (Some(0), Some(_)) => 1
        case _ =>
          math.abs(e._1.get - e._2.get)
      }}.sum + shift
  }
}
