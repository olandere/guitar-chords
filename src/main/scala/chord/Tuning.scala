package chord

case class Tuning(semitones: List[Int], root: Note) {

  override def toString: String = {
    val revmap = retune(this).map(e => (e._2, e._1))
    val nmap = revmap.withDefault { n => revmap(norm(n - 1)) + "#" }
    semitones.map {
      nmap
    }.mkString(" ")
  }

  def numStrings: Int = semitones.length

  def notes: List[Note] = {
    val revmap = retune(this).map(e => (e._2, e._1))
    val nmap = revmap.withDefault { n => Note(revmap(norm(n - 1)).name, Sharp()) }
    semitones.map {
      nmap
    }
  }
}

object Tuning {

  val StandardTuning = Tuning(List(0, 5, 10, 3, 7, 0), Note("E"))
  val DADGAD = Tuning("D A D G A D")

  def apply(semitones: List[Int], root: Note): Tuning = {
    new Tuning(semitones, root)
  }

  // notes is space delimited - "E A D G B E", note lowest to highest
  def apply(notes: String): Tuning = {
    apply(delimitedToList(notes).map(_.capitalize))
  }

  def apply(notes: List[String]): Tuning = {
    val root = Note(notes.head)
    val semitones = notes.map { n => norm(NOTE_MAP(Note(n)) - NOTE_MAP(root)) }
    new Tuning(semitones, root)
  }
}


