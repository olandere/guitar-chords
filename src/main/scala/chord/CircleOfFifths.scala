package chord

object CircleOfFifths {

  //need to handle unicode # and b signs


  private val fifths = "F,C,G,D,A,E,B".split(",")
  private val sharps = (fifths ++ fifths.map(_ + "#") ++ fifths.map(_ + DOUBLE_SHARP)).tail
  private val flats = "C,F".split(",") ++ fifths.reverse.map(_ + "b") ++ fifths.reverse.map(_ + DOUBLE_FLAT)
 // private val flats = "C,F,Bb,Eb,Ab,Db,Gb,Cb,Fb".split(",")
  private val allNotes = (CircleOfFifths.flats.reverse ++ CircleOfFifths.sharps).distinct
   val mapping = {
    val m = allNotes.toList.drop(12).zip(allNotes.toList)
     m.toMap.orElse(m.map(_.swap).toMap)
  }

  private val majScale = List(0, 2, 4, -1, 1, 3, 5)
  private val minScale = List(0, 2, -3, -1, 1, -4, -2)

  private def normalize(key: String) = key.replace("♯", "#").replace("♭", "b")

  private val keyMap = sharps.zipWithIndex.toMap.mapValues(v => Sharp(v)) ++
    flats.zipWithIndex.toMap.mapValues(v => Flat(v))

  def relativeMinor(key: String): String = sharps(sharps.indexOf(key) + 3)

  def numberOfSharps(key: String): Int = sharps.indexOf(key)

  def enharmonic(key: Note): Note = {
    key.enharmonic

//    val c = allNotes.indexOf("C")
//    val root = allNotes.indexOf(normalize(key))
//    if (root <= c) allNotes(root + 12) else allNotes(root - 12)
  }

  //def norm(i: Int) = if (i >= allNotes.length) i % 12 else i

  private def useEnharmonic(key: Note, center: String = "C"): Boolean = {
    math.abs(allNotes.indexOf(center) - allNotes.indexOf(normalize(key.toString))) > 7
  }

  def majorScale(key: Note): List[String] = {
    val root = if (useEnharmonic(key)) enharmonic(key) else key
    println(s"key: $key, root: $root")
    majScale.map(i => allNotes(allNotes.indexOf(normalize(root.toString)) + i))
  }

  def minorScale(key: Note): List[String] = {
    val root = if (useEnharmonic(key, "A")) key.enharmonic else key
    println(s"key: $key, root: $root")
    minScale.map(i => allNotes(allNotes.indexOf(normalize(root.toString)) + i))
  }

  trait KeySignature {}

  case class Sharp(num: Int) extends KeySignature

  case class Flat(num: Int) extends KeySignature

}