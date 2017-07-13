package chord

import org.scalatest._
import cats.implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

/**
  * Created by eolander on 1/4/15.
  */
class ChordSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "A chord" should "have correct semitones" in {
    Chord("Gdim7").semitones shouldBe List(0, 3, 6, 9)
    Chord("Cdim").semitones shouldBe List(0, 3, 6)
    Chord("Eb").semitones shouldBe List(0, 4, 7)
    Chord("Bb-7").semitones shouldBe List(0, 3, 7, 10)
    Chord("F#-7b5").semitones shouldBe List(0, 3, 6, 10)
    Chord("F9").semitones shouldBe List(0, 4, 7, 10, 2)
    Chord("AM13").semitones shouldBe List(0, 4, 7, 11, 2, 5, 9)
    Chord("C6").semitones shouldBe List(0, 4, 7, 9)
    Chord("Db+").semitones shouldBe List(0, 4, 8)
    Chord("B+11").semitones shouldBe List(0, 4, 8, 10, 2, 5)
    Chord("E7b9").semitones shouldBe List(0, 4, 7, 10, 1)
  }

  it should "be a correct shell chord" in {
    Chord("Bb-7").asShell.semitones shouldBe List(0, 3, 10)
    Chord("AM13").asShell.semitones shouldBe List(0, 4, 11, 9)
  }

  it should "have the correct degrees" in {
    val C6 = Chord("C6")
    val fingering = Chord.unapply("x 3 2 2 x 3")
    assert(!C6.asDegrees(fingering).show.split(" ").contains("13"))
    C6.asDegrees(fingering).show.split(" ") should contain("R")
  }

  it should "have the correct degrees with altered tuning" in {
    implicit val tuning = Tuning("D G B D")
    val G = Chord("G")
    val fingering = Chord.unapply("x 0 0 0")
    assert(!G.asDegrees(fingering).show.split(" ").contains("13"))
    G.asDegrees(fingering).show.split(" ") should contain("R")
  }

  it should "handle slash chords" in {
    implicit val tuning = Tuning.StandardTuning
    val chord = Chord("A/F")
    val fingering = Chord.unapply("1 x 2 0 1 x")
    chord.asDegrees(fingering).show
  }

  it should "handle power chords" in {
    implicit val tuning = Tuning.StandardTuning
    val G5 = Chord("G5")
    G5.toString shouldBe "G5"
    G5.semitones shouldBe List(0, 7)
    val fingering = Chord.unapply("3 5 5 x x x")
    G5.asDegrees(fingering).show shouldBe "R 5 R x x x"
  }

  it should "handle add9 chords" in {
    Chord("Gadd9").semitones shouldBe List(0, 4, 7, 2)
    Chord("G6add9").semitones shouldBe List(0, 4, 7, 9, 2)
    Chord("Gm6add9").semitones shouldBe List(0, 3, 7, 9, 2)
    Chord("Gadd9").asShell.semitones shouldBe List(0, 4, 2)
  }

  it should "handle add11 chords" in {
    Chord("Gadd11").semitones shouldBe List(0, 4, 7, 5)
    Chord("G6add11").semitones shouldBe List(0, 4, 7, 9, 5)
    Chord("Gm6add11").semitones shouldBe List(0, 3, 7, 9, 5)
    Chord("Gadd11").asShell.semitones shouldBe List(0, 4, 5)
  }

  it should "handle multiple adds" in {
    Chord("Gadd9add11").semitones shouldBe List(0, 4, 7, 2, 5)
  }

  it should "handle banjo chords" in {
    implicit val tuning = Tuning("D G B D")
    val c = Chord("C")
    c.semitones shouldBe List(0, 4, 7)
    val fingering = Chord.unapply("2 0 1 x")
    c.asDegrees(fingering).show shouldBe "3 5 R x"
  }

  it should "handle frettings" in {
    implicit val tuning = Tuning.StandardTuning
    val result = List(Some(1), None, Some(2), Some(0), Some(1), None)
    Chord.unapply(" 1 x 2 0 1 x") shouldBe result
    Chord.unapply("1  x 2 0 1 x") shouldBe result
    Chord.unapply("1 x 2 0 1 x ") shouldBe result
    Chord.unapply("1x201x") shouldBe result
    Chord.unapply("1X201x") shouldBe result
  }

  it should "compute diffs" in {
    implicit val tuning = Tuning.StandardTuning
    val em = Chord.unapply("0 x 0 0 x 7")
    val c7 = Chord.unapply("0 3 x 0 0 x")
    val c = diff(em, c7)
    println(c)
  }

  it should "handle suspensions" in {
    Chord("Asus4").semitones shouldBe List(0, 5, 7)
    Chord("Asus2").semitones shouldBe List(0, 2, 7)
  }

  it should "handle #11 chords" in {
    Chord("C7#11").semitones shouldBe List(0, 4, 7, 10, 6)
  }

  it should "handle b13 chords" in {
    Chord("C7b13").semitones shouldBe List(0, 4, 7, 10, 8)
  }

  it should "handle random chords" in {
    forAll(ChordGenerator.chordGen) { (c) =>
      println(s"c: $c")
      Chord(c).semitones shouldBe List(0, 4, 7)
    }
  }
}
