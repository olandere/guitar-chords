//Ideas - handle altered tunings
//Ideas - handle chord progressions - find nearest chords
//todo - allow notes to be dropped - e.g. no 5

class Chord(val root: String, val triad: String, val quality: String, val extension: Int, val alteration: String, val altRoot: String) {
	val INT_MAP = Map("1" -> 0, "3" -> 4, "5" -> 7, "6" -> 9, "7" -> 11, "9" -> 2, "11" -> 5, "13" -> 9)
	val NOTE_MAP: Map[String, Int] = Map("E" -> 0, "F" -> 1, "G" -> 3, "A" -> 5, "B" -> 7, "C" -> 8, "D" -> 10).withDefault{r => 
		if (r.endsWith("b")||r.endsWith("♭")) {
			-1 + NOTE_MAP(r(0).toString)
		} else {
			1 + NOTE_MAP(r(0).toString)
		}}
	val tuning = List(0,5,10,3,7,0)
	
	def intervals:List[String] = {
		
		def performAlterations(ints: List[String]) = {
			def substitute(ints: List[String], alts: List[String]): List[String] = {
				println(s"ints: ${ints}, alts: ${alts}")
				if (alts.isEmpty) {ints}
				else if (ints.isEmpty) {alts}
				else {
					if (alts.head.endsWith(ints.head)) {
						alts.head :: substitute(ints.tail, alts.tail)
					} else {
						ints.head :: substitute(ints.tail, alts)
					}
				}
			}
			
			if (alteration.isEmpty) {
				ints
			} else {
				val altMatch = """([#b♯♭](5|9))""".r
				substitute(ints, altMatch.findAllIn(alteration).toList)
			}
		}
		
		val ints: List[String] = (List("1") :+ (triad match {
			case "min" | "dim" => "♭3"
			case _ => "3"
		}) :+ (triad match {
			case "dim" => "♭5"
			case "aug" => "♯5"
			case _ => "5"})) ++ 
	  (if (extension == 0) Nil else if (extension == 6) {
			List("6")
		} else {
			if (quality == "maj") {
			  List("7")
		    } else List("♭7")
		}) ++ (if (extension > 7) {Range(9,extension+1,2).toList.map(_.toString)} else Nil)
		performAlterations(ints)
	}
		
	def semitones:List[Int] = intervals.map(INT_MAP.withDefault{i => 
		if (i.startsWith("b")||i.startsWith("♭")) {
			-1 + INT_MAP(i(1).toString)
		} else {
			1 + INT_MAP(i(1).toString)
		}
	})	
	
	def allChords(fretSpan:Int = 6):List[List[Option[Int]]] = {
		def withinSpan(c:List[Option[Int]]):Boolean = {
			val m = c.filter{_ != None}.map{_.get}
			scala.math.abs(m.max-m.min) < fretSpan
		}
		
		def adjustOctave(c:List[Option[Int]]): List[Option[Int]] = {
			val m = c.filter{_ != None}.map{_.get}
			if (m.min < 0) c.map{_.map{x:Int=>x+12}} else c
		}
		
		def transpose(c:List[Option[Int]]): List[Option[Int]] = 
			c.map{_.map{_ + NOTE_MAP(root)}}
		
		
		semitones.map{Some(_)}.padTo(6, None).permutations.map(_.zip(tuning)).map{
			_.map{a:Tuple2[Option[Int],Int]=>a._1.flatMap{
				x =>
		          val d = x - a._2 
		          if (d< -6) Some(d+12) else if (d >6) Some(d-12) else Some(d)
		}}}.filter{withinSpan}.map(transpose).map(adjustOctave)
	}
		
	def printChords(fretSpan:Int = 6) = {	
		def adjustOctave(c:List[Option[Int]]):List[Option[Int]] = {
			val m = c.filter{_ != None}.map{_.get}
			if (m.min < 0) c.map{_.map{x:Int=>x+12}} else c
		}
		
	  def printChord(c: List[Option[Int]]) = {
		//adjustOctave(c.map{o => o.map{_ + NOTE_MAP(root)}}).map{_.getOrElse("x")}
		c.map{_.getOrElse("x")}
	  }	
	  allChords(fretSpan).map(printChord)
	}
}

object Chord {
	val chordMatch = """([ABCDEFG][♯#b♭]?)(m|-|\+|aug|dim)?(M|maj)?(6|7|9|11|13)?(([♯#b♭](5|9))*)(/([ABCDEFG][♯#b♭]?))?""".r
	
	def triad(s: String) = {
		s match {
			case "m" | "-" => "min"
			case "aug" | "dim" => s
			case "+" => "aug"
			case _ => "maj"
		}
	}
	
	def seventh(s: String) = {
		s match {
			case "M" | "maj" => "maj"
			case _ => "dom"
		}
	}
	
	def apply(s: String) = {
		val chordMatch(root, t, qual, ext, alt, _, _, _, altRoot) = s
		new Chord(root, triad(t), seventh(qual), Option(ext).getOrElse("0").toInt, Option(alt).getOrElse(""), altRoot)
	}
}