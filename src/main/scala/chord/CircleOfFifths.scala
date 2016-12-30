package chord

object CircleOfFifths {

	//need to handle unicode # and b signs

	val sharps = "C,G,D,A,E,B,F#,C#,G#,D#,A#,E#,B#,F\uD834\uDD2A".split(",")
	val flats = "C,F,Bb,Eb,Ab,Db,Gb,Cb,Fb".split(",")
	val allNotes = (CircleOfFifths.flats.reverse ++ CircleOfFifths.sharps).distinct

	val majScale = List(0, 2, 4, -1, 1, 3, 5)
	val minScale = List(0, 2, -3, -1, 1, -4, -2)

  def normalize(key:String):String = key.replace("♯","#").replace("♭", "b")

	val keyMap = sharps.zipWithIndex.toMap.mapValues(v => Sharp(v)) ++ 
	flats.zipWithIndex.toMap.mapValues(v => Flat(v))
	
	def relativeMinor(key:String):String = sharps(sharps.indexOf(key)+3)

	def numberOfSharps(key:String):Int = sharps.indexOf(key)

    //def norm(i: Int) = if (i >= allNotes.length) i % 12 else i

	def majorScale(key:String) = majScale.map(i => allNotes(allNotes.indexOf(normalize(key)) + i))

	def minorScale(key:String) = minScale.map(i => allNotes(allNotes.indexOf(key) + i))

    trait KeySignature {}

	case class Sharp(val num: Int) extends KeySignature

	case class Flat(val num: Int) extends KeySignature
}