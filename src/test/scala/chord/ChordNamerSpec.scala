package chord

import cats.implicits._
import chord.Operations._
import org.scalatest._

/**
 * Created by eolander on 2/17/15.
 */
class ChordNamerSpec extends FlatSpec with Matchers {

  "ChordNamer" should "identify chords" in {
    //assert(ChordNamer(chords("1 3 3 1 1 1")) == "m")
    //assert(ChordNamer(List(0,7,10,3,7)) == "m7")

  }

  it should "determine inversions" in {
    ChordNamer(Chord.unapply("1 3 3 1 1 1")).determineInversion shouldBe "root"
    ChordNamer(Chord.unapply("x 0 2 2 2 0")).determineInversion shouldBe "root"
    ChordNamer(Chord.unapply("x 3 2 2 1 0")).determineInversion shouldBe "1st"
    ChordNamer(Chord.unapply("0 0 2 2 2 0")).determineInversion shouldBe "2nd"

    ChordNamer(Chord.unapply("5 x 5 5 5 x")).determineInversion shouldBe "root"
    ChordNamer(Chord.unapply("5 x 4 5 5 x")).determineInversion shouldBe "1st"
    ChordNamer(Chord.unapply("5 x 6 6 5 x")).determineInversion shouldBe "root"
    ChordNamer(Chord.unapply("x 3 5 4 5 x")).determineInversion shouldBe "root"
    ChordNamer(Chord.unapply("x 3 4 2 4 x")).determineInversion shouldBe "root"
    ChordNamer(Chord.unapply("x 3 3 2 3 x")).determineInversion shouldBe "3rd"
    ChordNamer(Chord.unapply("x 3 4 2 3 x")).determineInversion shouldBe "3rd"
  }

  it should "handle extensions" in {
    ChordNamer(Chord.unapply("x 7 6 7 7 x")).determineInversion shouldBe "root"
  }

  it should "respell" in {
    ChordNamer("x 3 2 2 1 0").determineInversion shouldBe "root"
    ChordNamer("0 0 2 2 2 0").determineInversion shouldBe "root"
    ChordNamer("5 x 4 5 5 x")
  }

  it should "detect quality" in {
    ChordNamer("x 3 2 2 1 0").quality shouldBe "m"
  }

  it should "name chords" in {
    ChordNamer("x 3 2 2 1 0").toString shouldBe "Am"
    ChordNamer("x 0 2 2 2 0").toString shouldBe "A"
    ChordNamer("x 0 2 0 2 0").toString shouldBe "A7"
    ChordNamer("x 0 2 0 1 0").toString shouldBe "Am7"
    ChordNamer("5 x 5 5 5 x").toString shouldBe "Am7"
    ChordNamer("5 x 6 6 5 x").toString shouldBe "AM7"
    ChordNamer("5 x 5 6 6 x").toString shouldBe "A+7"
    ChordNamer("7 x 7 7 6 x").toString shouldBe "Bm7♭5"
    ChordNamer("x 7 6 7 6 x").toString shouldBe "E7♭9"
    ChordNamer("5 x 5 6 7 x").toString shouldBe "A13"
    ChordNamer("x 0 8 6 8 0").toString shouldBe "A7♭9"
    ChordNamer("x x 0 2 3 0").toString shouldBe "Dsus2"
    ChordNamer("x 0 2 0 3 0").toString shouldBe "A7sus4"
    ChordNamer("x 3 4 0 3 0").toString shouldBe "Cadd9♯11"
    ChordNamer("x 3 3 2 3 x").toString shouldBe "Dm7"
    ChordNamer("x 3 4 2 3 x").toString shouldBe "D7"
    ChordNamer("x 3 4 5 4 x").toString shouldBe "Cdim"
    ChordNamer("x321xx").toString shouldBe "C+"
  }

  it should "name chords in altered tunings" in {
    implicit val tuning = Tuning("D A D G A D")
    ChordNamer("2 2 2 0 2 2").toString shouldBe "Em"
  }

  it should "name chords in altered tunings with accidentals" in {
    implicit val tuning = Tuning("C# G# C# G# C# E")
    ChordNamer("xx3004").toString shouldBe "C♯m"
  }

  it should "name unusual chords in ukulele tuning" in {
    implicit val tuning = TuningParser("GCEA")
    ChordNamer("2 0 2 3").toString shouldBe "F♯dim"
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

  it should "handle jimi hendrix chord" in {
    ChordNamer("x7678x").toString shouldBe "E7♯9"
  }
}
