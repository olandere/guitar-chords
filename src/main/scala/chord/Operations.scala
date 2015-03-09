package chord

import chord._

/**
 * Created by eolander on 12/25/14.
 */
object Operations {

  def generatePermutations(chord: Chord)(implicit tuning: Tuning) = {
    if (chord.altRoot.isDefined) {
      val altInt = Some(norm(NOTE_MAP(chord.altRoot.get)-NOTE_MAP(chord.root)))
      chord.semitones
      .map {Some(_)}
      .padTo(tuning.numStrings - 1, None)
      .permutations.map{x => altInt :: x}
    } else {
      chord.semitones
      .map {Some(_)}
      .padTo(tuning.numStrings, None)
      .permutations
    }
  }

  private def withinSpan(fretSpan: Int)(c: FretList): Boolean = {
    val m = c.filter {_ != None}.map {_.get}.filter{_>0}
    if (m.isEmpty) {
      false
    } else {
      scala.math.abs(m.max - m.min) < fretSpan
    }
  }

  def fingerings(chord: Chord, fretSpan: Int = 6)(implicit tuning: Tuning): List[FretList] = {

    def adjustOctave(c: FretList) = {
      val m = c.filter {_ != None}.map {_.get}
      val r = if (m.min < 0) {
        c.map {_.map { x: Int => x + 12}}
      } else if (m.min >= 12) {
        c.map {_.map { x: Int => x - 12}}
      } else {
        c
      }
      val m2 = r.filter {_ != None}.map {_.get}.sorted
      val span = m2.max - m2.min
      if (span > 6) {
        val diffs = m2.zip(m2.tail).map{case (x, y)=>scala.math.abs(x-y)}
        val dm = diffs.max
        if (dm == diffs.head) { //outlier is lowest note, need to raise it
          val r1 = r.map {_.map{n => if (n == m2.min) n+12 else n}}
          if (r1.filter {_ != None}.map {_.get}.min >= 12) {
            r1.map {_.map { x: Int => x - 12}}
          } else r1
        } else { // outlier is highest note, need to lower it
          r.map {_.map{n => if (n == m2.max && n > 11) n-12 else n}}
        }
      } else r
    }

    def transpose(c: FretList) = c.map {_.map {_ + retune(tuning)(chord.root)}}

    def rootPosition(c: FretList): Boolean =
      if (c.isEmpty) false else
      if (c.head.isDefined) {
        c.head.get == 0
      } else {
        rootPosition(c.tail)
      }



//    chord.semitones
//    .map {Some(_)}
//    .padTo(6, None)
//    .permutations  //.filter(rootPosition)
   // generatePermutations.foreach(c => println(c))
    chord.filterFingerings(
    generatePermutations(chord)
    .map(_.zip(tuning.semitones)).map {_.map { a: Tuple2[Option[Int], Int] => a._1.map { x => norm(x - a._2) }}}
    .toList
    .map { c => adjustOctave(transpose(c))}.filter(withinSpan(fretSpan))
    .sorted(fretListOrder.toScalaOrdering))
  }

  def combine(c1: FretList, c2: FretList): Option[FretList] = {
    def helper(c1: FretList, c2: FretList, acc: FretList): Option[FretList] = {
      if (c1.isEmpty) Some(acc)
        else if (c1.head.isDefined) {
          if (c2.head.isDefined) {
            if (c1.head == c2.head) {
              helper(c1.tail, c2.tail, acc :+ c1.head)
            } else {
              None
            }
          } else {
            helper(c1.tail, c2.tail, acc :+ c1.head)
          }
      } else {
        helper(c1.tail, c2.tail, acc :+ c2.head)
      }
    }
    helper(c1, c2, List())
  }

  def condense(chords: List[FretList], fretSpan: Int): List[FretList] = {
    def helper(chords: List[FretList], chord: FretList, acc: List[FretList]): List[FretList] = {
      if (chords.isEmpty) acc :+ chord
      else {
        val nc = combine(chords.head, chord)
        if (nc.isDefined) helper(chords.tail, nc.get, acc)
        else helper(chords.tail, chords.head, acc :+ chord)
      }
    }
    helper(chords.tail, chords.head, List()).filter(withinSpan(fretSpan))
  }

  def chords(chord: String)(implicit tuning: Tuning) = {
    def getRoot(fl: FretList, tuning: List[Int]): Int = {
      if (fl.head.isDefined) norm(fl.head.get+tuning.head) else getRoot(fl.tail, tuning.tail)
    }

    def intervals(fl: FretList, root: Int) = {
      fl.zip(tuning.semitones).map{case (Some(n), t) => Some(norm(n+t-root)); case (None, _) => None}//.filterNot(_ == None).map(_.get)//.sorted
    }
    val fl = Chord.unapply(chord)
    val ints = intervals(fl, getRoot(fl, tuning.semitones))
    println(ints)
    val namer = ChordNamer(chord)//, ints.map{_.toList}.flatten)
    println(s"$namer")
    (namer.intervals map (_.map(SEMI_TO_INT).getOrElse("x")), namer.toString)//.mkString(" ")
  }

  def progression(chords: List[Chord], fretSpan: Int)(implicit tuning: Tuning) = {

    def proximitySort(f: FretList, fl: List[FretList]):List[FretList] = {
      fl.map{fi => (fi, diff(f, fi))}.sortBy{_._2}.map{_._1}
    }

    def helper(chord: FretList, fl: List[List[FretList]]): List[FretList] = {
      if (fl.isEmpty) List(chord)
      else {
        val closest = proximitySort(chord, fl.head).head
        chord :: helper(closest, fl.tail)
      }
    }

    val fingeringList = chords.map{c=>fingerings(c, fretSpan)}
    fingeringList.head.map{c => helper(c, fingeringList.tail)}

    // initially 2 chords only
//    val c1 = chords.head
//    val c2 = chords.tail.head
//    val c1Fingerings = fingerings(c1, fretSpan)
//    val c2Fingerings = fingerings(c2, fretSpan)
//
//    for {
//      f1 <- c1Fingerings
//      f2 <- proximitySort(f1, c2Fingerings).take(1)
//      //fingerings(c2, fretSpan).map{f => (f, c1.diff(f1, f))}.sortBy{_._2}.map{_._1}.take(1)
//    } yield (f1, f2)
  }
}
