package chord

import scala.annotation.tailrec

/**
 * Created by eolander on 12/25/14.
 */
object Operations {

  def generatePermutations(chord: Chord, withRepetition: Boolean = true)(implicit tuning: Tuning): Iterator[FretList] = {
    val semitones = (if (chord.altRoot.isDefined && !chord.semitones.contains(chord.altRootInterval.get)) {
      chord.semitones ++ chord.altRootInterval.toList
    } else {
      chord.semitones
    }).map{Some(_)}
    if (withRepetition) {
      val (essential, optional) = chord.splitIntervals(tuning.numStrings)
  //    println(s"$semitones, $essential, $optional")
  //    println(semitones.filterNot(n => optional.contains(n.get)))
      combWithRepeat((if (optional.isEmpty) essential else optional).map{Some(_)} ++ List(None),
                     tuning.numStrings - essential.length).map{l => l ++ semitones.filterNot(n => optional.contains(n.get))}
      .flatMap(_.permutations).toIterator}
    else semitones.padTo(tuning.numStrings, None).permutations
  }

  private def withinSpan(fretSpan: Int)(c: FretList): Boolean = {
    val m = c.filter {_.isDefined}.map {_.get}.filter{_>0}
    if (m.isEmpty) {
      false
    } else {
      scala.math.abs(m.max - m.min) < fretSpan
    }
  }

  def adjustOctave2(c: FretList): FretList = {
    def fretspan(f: FretList) = {
      val m = f.filter {x => x.isDefined}.map {_.get}
      m.max-m.min
    }

    def hasOpenStrings(c: FretList): Boolean = {
      c.contains(Some(0))
    }

    def raiseStrings(c: FretList): FretList = {
      val max = c.max.get
      c.map{_.map {n => if (n != max && math.abs(max - n) > math.abs(max - (n + 12))) n + 12 else n}}
//      c.map { case Some(0) => Some(12)
//      case x => x
//            }
    }

    val result = c.map{_.map{norm}}
  //  if (hasOpenStrings(result)) {
      val raised = raiseStrings(result)
      if (fretspan(raised) < fretspan(result)) raised else result
  //  } else
    //  result
  }

  @tailrec
  def adjustOctave(c: FretList): FretList = {

    def fretspan(f: FretList): Int = {
      val m = f.filter {x => x.isDefined && x.get > 0}.map {_.get}
      m.max-m.min
    }

    val m = c.filter {_.isDefined}.map {_.get}
    val r = if (m.min < 0) {
      c.map {_.map { x: Int => x + 12}}
    } else if (m.min >= 12) {
      c.map {_.map { x: Int => x - 12}}
    } else {
      c
    }
    val m2 = r.filter {x => x.isDefined}.map {_.get}.sorted
    val span = m2.max - m2.filter{_>0}.min
    val result = if (span > 6) {
      val diffs = m2.zip(m2.tail).map{case (x, y)=>scala.math.abs(x-y)}
      val dm = diffs.max
      if (dm == diffs.head) { //outlier is lowest note, need to raise it
      val r1 = r.map {_.map{n => if (n == m2.min) n+12 else n}}
        if (r1.filter {_.isDefined}.map {_.get}.min >= 12) {
          r1.map {_.map { x: Int => x - 12}}
        } else r1
      } else { // outlier is highest note, need to lower it
        r.map {_.map{n => if (n == m2.max && n > 11) n-12 else n}}
      }
    } else r
//    println(s"$c, $result")
    if (result == c || fretspan(result) >= fretspan(c)) c else adjustOctave(result)
  }

  def fingerings(chord: Chord, fretSpan: Int = 6, jazzVoicing: Boolean = false)(implicit tuning: Tuning): List[FretList] = {

    def transpose(c: FretList) = c.map {_.map {n=>n + retune(tuning)(chord.root)}}

    def alteredRoot(c: FretList): Boolean = {
      if (chord.altRootInterval.isDefined) {
        c.dropWhile(_.isEmpty).head == chord.altRootInterval
      } else true
    }

    def rootPosition(c: FretList): Boolean =
      if (c.isEmpty) {false} else {
        if (c.head.isDefined) {
          c.head.get == 0
        } else {
          rootPosition(c.tail)
        }
      }



//    chord.semitones
//    .map {Some(_)}
//    .padTo(6, None)
//    .permutations  //.filter(rootPosition)
   // generatePermutations.foreach(c => println(c))
    chord.filterFingerings({
    val r = generatePermutations(chord, !jazzVoicing).toSet.filter(alteredRoot)
    .map(_.zip(tuning.semitones)).map {_.map { a: Tuple2[Option[Int], Int] => a._1.map { x => norm(x - a._2) }}}
    .toList
//    r.foreach{x=>println(x)}
//      println("---")
    val r2 = r.map { c =>
//      println(s"$c, ${transpose(c)}, ${adjustOctave2(transpose(c))}")
      adjustOctave2(transpose(c))}
                           //  r2.foreach{x=>println(x)}
             r2.filter(withinSpan(fretSpan))
    .sorted(fretListOrder.toOrdering)
                           })
  }

  def combine(c1: FretList, c2: FretList): Option[FretList] = {
    def helper(c1: FretList, c2: FretList, acc: FretList): Option[FretList] = {
      if (c1.isEmpty) {Some(acc)} else if (c1.head.isDefined) {
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
      if (chords.isEmpty) {acc :+ chord} else {
        val nc = combine(chords.head, chord)
        if (nc.isDefined) {helper(chords.tail, nc.get, acc)} else {helper(chords.tail, chords.head, acc :+ chord)}
      }
    }
    helper(chords.tail, chords.head, List()).filter(withinSpan(fretSpan))
  }

  def chords(chord: String)(implicit tuning: Tuning) = {
    def getRoot(fl: FretList, tuning: List[Int]): Int = {
      if (fl.head.isDefined) {norm(fl.head.get+tuning.head)} else {getRoot(fl.tail, tuning.tail)}
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

  def progression(chords: List[Chord], fretSpan: Int)(implicit tuning: Tuning): List[List[FretList]] = {

    def proximitySort(f: FretList, fl: List[FretList]): List[FretList] = {
      fl.map{fi => (fi, diff(f, fi))}.sortBy{_._2}.map{_._1}
    }

    def helper(chord: FretList, fl: List[List[FretList]]): List[FretList] = {
      if (fl.isEmpty) {List(chord)} else {
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

  def fingering(scale: Scale)(implicit tuning: Tuning): Array[List[Int]] = {
    for {
     root <- tuning.toString.split(" ")
     frets = (scale.intervals ++ scale.intervals.map(_ + 12)).map {_ + retune(root)(scale.root)}
    }
    yield (frets ++ frets.map(_ % 12)).sorted.distinct.filter(_ < 16)
  }

  def possibleScales(chord: Chord)(implicit tuning: Tuning) = {
    Scale.allScales(chord.root).filter(_.containsChord(chord))
  }

  def combWithRepeat[T](input: List[T], length: Int) = {

    def helper(inSet: List[T], result: List[T], len: Int): List[List[T]] = {
      if (len == 0) List(result)
      else {
        helper(inSet, result :+ inSet.head, len - 1) ++
        (if (inSet.length > 1) helper(inSet.tail, result, len) else Nil)
      }
    }
    require (length > 0)
    helper(input, Nil, length)
  }
}
