package chord

class Tuning(val semitones: List[Int], val root: String) {

  override def toString = {
    val revmap = retune(this).map(e => (e._2, e._1))
    val nmap = revmap.withDefault { n => revmap(n - 1) + "#"}
    semitones.map {nmap}.mkString(" ")
  }

  def numStrings = semitones.length
}

object Tuning {

  val StandardTuning = Tuning(List(0, 5, 10, 3, 7, 0), "E")
  val DADGAD = Tuning("D A D G A D")

  def apply(semitones: List[Int], root: String): Tuning = {
    new Tuning(semitones, root)
  }

  // notes is space delimited - "E A D G B E", note lowest to highest
  def apply(notes: String): Tuning = {
    val noteArr = delimitedToList(notes).map(_.capitalize)
    val root = noteArr.head
    val semitones = noteArr.toList.map { n => norm(NOTE_MAP(n) - NOTE_MAP(root))}
    new Tuning(semitones, root)
  }
}


