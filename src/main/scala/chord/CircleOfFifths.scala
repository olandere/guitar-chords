package chord

object CircleOfFifths {

  private val fifths = "F,C,G,D,A,E,B".split(",")
  private val sharps = (fifths ++ fifths.map(_ + "#") ++ fifths.map(_ + DOUBLE_SHARP)).tail
  private val flats = "C,F".split(",") ++ fifths.reverse.map(_ + "b") ++ fifths.reverse.map(_ + DOUBLE_FLAT)
  val flatKeys: Seq[Note] = flats.tail.takeWhile(k => k == "F" || k.contains("b")).take(7).map(Note.apply).toSeq
  private val allNotes = (CircleOfFifths.flats.reverse ++ CircleOfFifths.sharps).distinct.map(Note.apply)

  def fifths(n: Note): LazyList[Note] = n #:: fifths(n.raise(Interval("P5")))

  //private val allNotes =

  val mapping: PartialFunction[Note, Note] = {
    val m = allNotes.toList.drop(12).zip(allNotes.toList)
    m.toMap.orElse(m.map(_.swap).toMap)
  }

  private val majScale = List(0, 2, 4, -1, 1, 3, 5)
  val majScaleDegrees: Seq[Degree] = List("R", "2", "3", "4", "5", "6", "7").map(Degree.apply)
  private val minScale = List(0, 2, -3, -1, 1, -4, -2) // R 2 b3 4 5 b6 b7
  val minScaleDegrees: Seq[Degree] = List("R", "2", "b3", "4", "5", "b6", "b7").map(Degree.apply)

  def relativeMinor(key: String): String = sharps(sharps.indexOf(key) + 3)

  def numberOfSharps(key: String): Int = sharps.indexOf(key)

  def enharmonic(key: Note): Note = {
    key.enharmonic
  }

  private def useEnharmonic(key: Note, center: Note = Note("C")): Boolean = {
    math.abs(allNotes.indexOf(center) - allNotes.indexOf(key)) > 7
  }

  def majorScale(key: Note): Seq[Note] = {
    val root = if (useEnharmonic(key)) key.enharmonic else key
    Major(key).notes
  }

  def minorScale(key: Note): List[Note] = {
    val root = if (useEnharmonic(key, Note("A"))) key.enharmonic else key
    minScale.map(i => allNotes(allNotes.indexOf(root) + i))
  }

  trait KeySignature {}

  case class Sharp(num: Int) extends KeySignature

  case class Flat(num: Int) extends KeySignature
}
