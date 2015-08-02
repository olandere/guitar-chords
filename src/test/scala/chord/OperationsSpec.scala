package chord

import org.scalatest.{ShouldMatchers, FlatSpec}
import Operations._
import scalaz._, syntax.show._
import scala.language.implicitConversions

/**
 * Created by eolander on 12/28/14.
 */
class OperationsSpec extends FlatSpec with ShouldMatchers {

  "Fingerings" should "generate known fingerings" in {

    assert(fingerings(Chord("E-9").asShell, 3).map{_.shows}.contains("x 7 5 7 7 x"))
    assert(fingerings(Chord("D-9").asShell, 3).map{_.shows}.contains("x 5 3 5 5 x"))
    assert(fingerings(Chord("FM7"), 4).map{_.shows}.contains("x x 3 2 1 0"))
    assert(fingerings(Chord("AM7"), 4).map{_.shows}.contains("x x 7 6 5 4"))
    assert(fingerings(Chord("BM7"), 4).map{_.shows}.contains("7 x 8 8 7 x"))
    assert(fingerings(Chord("A"), 3).map{_.shows}.contains("0 x x 6 x 5"))

    assert(fingerings(Chord("E"), 4).forall(c => c.forall(_.forall(_>=0))))
  }

  it should "handle banjo chords" in {
    implicit val tuning = Tuning("D G B D")
    assert(fingerings(Chord("A"), 4).map{_.shows}.contains("x 2 2 2"))
    assert(fingerings(Chord("E7").asShell, 4).map{_.shows}.contains("x 1 3 2"))
    assert(fingerings(Chord("C")).map{_.shows}.forall(c=>c.exists(_=='x')))  //todo: should have exactly one 'x'
  }

  it should "handle slash chords" in {
    assert(fingerings(Chord("A/D"), 4).map{_.shows}.contains("x x 12 9 10 9"))
  }

  it should "understand add 9 chords" in {
    assert(fingerings(Chord("Cadd9"), 4).map{_.shows}.contains("x 3 2 0 3 x"))
    assert(fingerings(Chord("Cmadd9"), 4).map{_.shows}.contains("x 3 1 0 3 x"))
    assert(fingerings(Chord("C6add9"), 4).map{_.shows}.contains("x 3 2 0 3 5"))
  }

  it should "understand add 11 chords" in {
    assert(fingerings(Chord("Cadd11"), 4).map{_.shows}.contains("x 3 3 0 5 x"))
    assert(fingerings(Chord("Cmadd11"), 4).map{_.shows}.contains("x 3 3 x 4 3"))
    assert(fingerings(Chord("C6add11"), 4).map{_.shows}.contains("x 3 3 0 5 5"))
  }

  it should "unfortunately, handle power chords" in {
    assert(fingerings(Chord("A5"), 4).map{_.shows}.contains("5 7 7 x x x"))
  }

  it should "analyze fingerings" in {
    chords("1 3 3 2 1 1")
    chords("1 3 3 1 1 1")
    chords("x 0 2 0 1 0")
    chords("4 x 4 4 4 x")
  }

  it should "chart progressions" in {
    implicit val tuning = Tuning.StandardTuning
    val p = progression(List(Chord("Em7"), Chord("CM7")), 3)
    println(p)
  }

  it should "handle root position filter" in {
    val aM7 = new Chord(Chord("AM7")) with RootPosition
    assert(fingerings(aM7, 4).map{_.shows}.contains("x x 7 6 5 4"))
  }

  it should "handle drop2 position filter" in {
    val cM7 = new Chord(Chord("CM7")) with Drop2
    assert(fingerings(cM7, 4).map{_.shows}.contains("3 3 2 4 x x"))
    assert(fingerings(cM7, 4).size == 12)

    val c7 = new Chord(Chord("C7")) with Drop2
    assert(fingerings(c7, 4).size == 12)
  }

  it should "have correct drop2 fingerings" in {
    val c7 = new Chord(Chord("C7")) with Drop2
    val voicing3to6 = Set("3 3 2 3 x x", "6 7 5 5 x x", "8 10 8 9 x x", "12 13 10 12 x x")
    val voicing2to5 = Set("x 10 10 9 11 x", "x 1 2 0 1 x", "x 3 5 3 5 x", "x 7 8 5 8 x")
    val voicing1to4 = Set("x x 5 5 5 6", "x x 8 9 8 8", "x x 10 12 11 12", "x x 2 3 1 3")
//    println(fingerings(c7, 4).map{_.shows})
    fingerings(c7, 5).map{_.shows}.toSet.intersect(voicing3to6) should be (voicing3to6)
    assert(fingerings(c7, 4).map{_.shows}.toSet.intersect(voicing2to5) == voicing2to5)
    assert(fingerings(c7, 4).map{_.shows}.toSet.intersect(voicing1to4) == voicing1to4)
  }

  it should "handle drop2and4 position filter" in {
    println("handle drop2and4 position filter")
    val dbM7 = new Chord(Chord("DbM7")) with Drop2and4
    assert(fingerings(dbM7, 5).map{_.shows}.contains("4 4 x 5 6 x"))
   // println("DbM7")
    //fingerings(dbM7, 5).foreach{c=>println(c.shows)}
    assert(fingerings(dbM7, 5).size == 8)

    val c7 = new Chord(Chord("C7")) with Drop2and4
    assert(fingerings(c7, 4).size == 8)
  //  println("C7")
   // fingerings(c7, 5).foreach{c=>println(c.shows)}

    val cM7 = new Chord(Chord("CM7")) with Drop2and4
    assert(fingerings(cM7, 5).size == 9)
   // println("CM7")
   // fingerings(cM7, 5).foreach{c=>println(c.shows)}
  }

  it should "handle modes" in {
    val EmajScale = MajorScale("E")
    val AmajScale = MajorScale("A")
    val FsDorian = Dorian("F#")
    val GsPhrygian = Phrygian("G#")
    val ALydian = Lydian("A")
    val BMixolydian = Mixolydian("B")
    val CsAeolian = Aeolian("C#")
    val DsLocrian = Locrian("D#")
    fingering(EmajScale) should be(
      Array(List(0, 2, 4, 5, 7, 9, 11, 12, 14), List(0, 2, 4, 6, 7, 9, 11, 12, 14),
            List(1, 2, 4, 6, 7, 9, 11, 13, 14), List(1, 2, 4, 6, 8, 9, 11, 13, 14),
            List(0, 2, 4, 5, 7, 9, 10, 12, 14), List(0, 2, 4, 5, 7, 9, 11, 12, 14)))

    fingering(AmajScale) should be(
      Array(List(0, 2, 4, 5, 7, 9, 10, 12, 14), List(0, 2, 4, 5, 7, 9, 11, 12, 14),
            List(0, 2, 4, 6, 7, 9, 11, 12, 14), List(1, 2, 4, 6, 7, 9, 11, 13, 14),
            List(0, 2, 3, 5, 7, 9, 10, 12, 14, 15), List(0, 2, 4, 5, 7, 9, 10, 12, 14)))

    fingering(FsDorian).head should be(fingering(EmajScale).head)
    fingering(GsPhrygian).head should be(fingering(EmajScale).head)
    fingering(ALydian).head should be(fingering(EmajScale).head)
    fingering(BMixolydian).head should be(fingering(EmajScale).head)
    fingering(CsAeolian).head should be(fingering(EmajScale).head)
    fingering(DsLocrian).head should be(fingering(EmajScale).head)

    assert (EmajScale.containsChord(Chord("EM7")))
    assert(!EmajScale.containsChord(Chord("Em7")))
    assert(!EmajScale.containsChord(Chord("A7")))
    assert(EmajScale.containsChord(Chord("AM7")))
    assert(EmajScale.containsChord(Chord("B7")))
    assert(ALydian.containsChord(Chord("EM7")))
    assert(FsDorian.containsChord(Chord("B7")))
  }

  it should "find possible scales" in {
    possibleScales(Chord("E7b9")).foreach(s => println(s"$s, ${s.relatedScale}"))
    possibleScales(Chord("G♯m7")).foreach(s => println(s"$s, ${s.relatedScale}"))
    possibleScales(Chord("Abm7")).foreach(s => println(s"$s, ${s.relatedScale}"))
  }

  it should "correctly adjust octaves" in {
    assert(adjustOctave2(Chord.unapply("x 7 8 17 8 x")) == Chord.unapply("x 7 8 5 8 x"))
    assert(adjustOctave2(Chord.unapply("x 19 8 17 8 x")) == Chord.unapply("x 7 8 5 8 x"))
  }
}
