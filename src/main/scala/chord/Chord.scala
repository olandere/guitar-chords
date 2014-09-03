//Ideas - handle altered tunings
//Ideas - handle chord progressions - find nearest chords
//todo - allow notes to be dropped - e.g. no 5
package chord

import scalaz._, syntax.show._, syntax.order._, scalaz.Ordering._
//import chord._

class Chord(val root: String, val triad: String, val quality: String, val extension: Int, val alteration: String, val altRoot: String) {
	import chord.Chord._
	
	val INT_MAP = Map("1" -> 0, "3" -> 4, "5" -> 7, "6" -> 9, "7" -> 11, "9" -> 2, "11" -> 5, "13" -> 9)
	val SEMI_TO_INT = Map(0 -> "R", 1 -> "♭9", 2-> "9", 3 -> "♭3", 4 -> "3", 5 -> "11", 6 -> "♭5", 7 -> "5", 8 -> "♯5", 9 -> "13",
	                      10 -> "♭7", 11 -> "7")
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
	
	def allChords(fretSpan:Int = 6):List[FretList] = {
		def withinSpan(c:FretList):Boolean = {
			val m = c.filter{_ != None}.map{_.get}
			scala.math.abs(m.max-m.min) < fretSpan
		}
		
		def adjustOctave(c:FretList) = {
			val m = c.filter{_ != None}.map{_.get}
			if (m.min < 0) c.map{_.map{x:Int=>x+12}} else c
		}
		
		def transpose(c: FretList) = 
			c.map{_.map{_ + NOTE_MAP(root)}}
		
		
		semitones.map{Some(_)}.padTo(6, None).permutations.map(_.zip(tuning)).map{
			_.map{a:Tuple2[Option[Int],Int]=>a._1.flatMap{
				x =>
		          val d = x - a._2 
		          if (d< -6) Some(d+12) else if (d >6) Some(d-12) else Some(d)
		}}}.filter(withinSpan).toList.map{c=>adjustOctave(transpose(c))}.sorted(fretListOrder.toScalaOrdering)
	}
		
	def printChords(fretSpan: Int = 6) = {	
	  allChords(fretSpan).map(_.shows)
	}
	
	def norm(x:Int) = (x+12) % 12
	
	def asDegrees(a: FretList) = {
		a.zip(tuning).map{case (f,s) => if (f.isDefined) Some(SEMI_TO_INT(norm(f.get + s - NOTE_MAP(root)))) else None}.shows
	}
	
//	 def diff(c: Chord, fretSpan: Int):Int = {
//	 		for {
	// 		  c1 <- allChords(fretSpan)
	// 		  c2 <- c.allChords(fretSpan)
	// 		  diff()
	// 		}
	// 	}
	
	def diff(a: FretList, b: FretList): Int = {
		a.zip(b).map{e => if (e._1 == None || e._2 == None) 0 else math.abs(e._1.get - e._2.get)}.foldLeft(0)(_+_)
	}
}

object Chord {
	
	type FretList = List[Option[Int]]
	type DegreeList = List[Option[String]]

	implicit val fretListShow: Show[FretList] = Show.shows(
	  _.map{_.getOrElse("x")}.mkString(" "))
	
	implicit val fretListOrder: Order[FretList] = Order.order((a, b) => {
	  def helper(fl1: FretList, fl2: FretList): Ordering = {
		if (fl1.isEmpty || fl2.isEmpty) EQ
	    else if (!fl1.head.isDefined) helper(fl1.tail, fl2)
	    else if (!fl2.head.isDefined) helper(fl1, fl2.tail)
	    else if (fl1.head.get == fl2.head.get) helper(fl1.tail, fl2.tail)
	    else if (fl1.head.get < fl2.head.get) LT
	    else GT
	  }
	  helper(a, b)	
	})
	
	implicit val degreeListShow: Show[DegreeList] = Show.shows(
	  _.map{_.getOrElse("x")}.mkString(" "))
	
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