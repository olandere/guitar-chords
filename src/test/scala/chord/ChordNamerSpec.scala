package chord

import chord.Operations._
import cats.implicits._
import org.scalatest._

/**
 * Created by eolander on 2/17/15.
 */
class ChordNamerSpec extends FlatSpec {

  "ChordNamer" should "identify chords" in {
    //assert(ChordNamer(chords("1 3 3 1 1 1")) == "m")
    //assert(ChordNamer(List(0,7,10,3,7)) == "m7")

  }

  it should "determine inversions" in {
    assert(ChordNamer(Chord.unapply("1 3 3 1 1 1")).determineInversion == "root")
    assert(ChordNamer(Chord.unapply("x 0 2 2 2 0")).determineInversion == "root")
    assert(ChordNamer(Chord.unapply("x 3 2 2 1 0")).determineInversion == "1st")
    assert(ChordNamer(Chord.unapply("0 0 2 2 2 0")).determineInversion == "2nd")

    assert(ChordNamer(Chord.unapply("5 x 5 5 5 x")).determineInversion == "root")
    assert(ChordNamer(Chord.unapply("5 x 4 5 5 x")).determineInversion == "1st")
    assert(ChordNamer(Chord.unapply("5 x 6 6 5 x")).determineInversion == "root")
    assert(ChordNamer(Chord.unapply("x 3 5 4 5 x")).determineInversion == "root")
    assert(ChordNamer(Chord.unapply("x 3 4 2 4 x")).determineInversion == "root")
    assert(ChordNamer(Chord.unapply("x 3 3 2 3 x")).determineInversion == "3rd")
    assert(ChordNamer(Chord.unapply("x 3 4 2 3 x")).determineInversion == "3rd")
  }

  it should "handle extensions" in {
    assert(ChordNamer(Chord.unapply("x 7 6 7 7 x")).determineInversion == "root")
  }

  it should "respell" in {
    assert(ChordNamer("x 3 2 2 1 0").determineInversion == "root")
    assert(ChordNamer("0 0 2 2 2 0").determineInversion == "root")
    ChordNamer("5 x 4 5 5 x")
  }

  it should "detect quality" in {
    assert(ChordNamer("x 3 2 2 1 0").quality == "m")
  }

  it should "name chords" in {
    assert(ChordNamer("x 3 2 2 1 0").toString == "Am")
    assert(ChordNamer("x 0 2 2 2 0").toString == "A")
    assert(ChordNamer("x 0 2 0 2 0").toString == "A7")
    assert(ChordNamer("x 0 2 0 1 0").toString == "Am7")
    assert(ChordNamer("5 x 5 5 5 x").toString == "Am7")
    assert(ChordNamer("5 x 6 6 5 x").toString == "AM7")
    assert(ChordNamer("5 x 5 6 6 x").toString == "A+7")
    assert(ChordNamer("7 x 7 7 6 x").toString == "Bm7♭5")
    assert(ChordNamer("x 7 6 7 6 x").toString == "E7♭9")
    assert(ChordNamer("5 x 5 6 7 x").toString == "A13")
    assert(ChordNamer("x 0 8 6 8 0").toString == "A7♭9")
    assert(ChordNamer("x x 0 2 3 0").toString == "Dsus2")
    assert(ChordNamer("x 0 2 0 3 0").toString == "A7sus4")
    assert(ChordNamer("x 3 4 0 3 0").toString == "Cadd9♯11")
    assert(ChordNamer("x 3 3 2 3 x").toString == "Dm7")
    assert(ChordNamer("x 3 4 2 3 x").toString == "D7")
    assert(ChordNamer("x321xx").toString == "C+")
  }

  it should "name chords in altered tunings" in {
    implicit val tuning = Tuning("D A D G A D")
    assert(ChordNamer("2 2 2 0 2 2").toString == "Em")
  }

  it should "name chords in altered tunings with accidentals" in {
    implicit val tuning = Tuning("C# G# C# G# C# E")
    assert(ChordNamer("xx3004").toString == "C♯m")
  }

  it should "name all inversions" in {
    val am7 = Chord("Am7")
    assert(fingerings(am7, 5).forall { c => ChordNamer(c.show).toString == "Am7" })

    val aM7 = Chord("AM7")
    assert(fingerings(aM7, 5).forall { c => ChordNamer(c.show).toString == "AM7" })

    val a7 = Chord("A7")
    assert(fingerings(a7, 5).forall { c => ChordNamer(c.show).toString == "A7" })

    val am7b5 = Chord("Am7♭5")
    assert(fingerings(am7b5, 5).forall { c => Set("Am7♭5", "Cm6")(ChordNamer(c.show).toString) })
  }
}
