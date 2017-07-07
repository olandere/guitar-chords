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
    assert(Chord("Gdim7").semitones == List(0,3,6,9))
    assert(Chord("Cdim").semitones == List(0,3,6))
    assert(Chord("Eb").semitones == List(0,4,7))
    assert(Chord("Bb-7").semitones == List(0,3,7,10))
    assert(Chord("F#-7b5").semitones == List(0,3,6,10))
    assert(Chord("F9").semitones == List(0,4,7,10,2))
    assert(Chord("AM13").semitones == List(0,4,7,11,2,5,9))
    assert(Chord("C6").semitones == List(0,4,7,9))
    assert(Chord("Db+").semitones == List(0,4,8))
    assert(Chord("B+11").semitones == List(0,4,8,10,2,5))
    assert(Chord("E7b9").semitones == List(0,4,7,10,1))
  }

  it should "be a correct shell chord" in {
    assert(Chord("Bb-7").asShell.semitones == List(0,3,10))
    assert(Chord("AM13").asShell.semitones == List(0,4,11,9))
  }

  it should "have the correct degrees" in {
    val C6 = Chord("C6")
    val fingering = Chord.unapply("x 3 2 2 x 3")
    assert(!C6.asDegrees(fingering).show.split(" ").contains("13"))
    assert(C6.asDegrees(fingering).show.split(" ").contains("R"))
  }

  it should "have the correct degrees with altered tuning" in {
    implicit val tuning = Tuning("D G B D")
    val G = Chord("G")
    val fingering = Chord.unapply("x 0 0 0")
    assert(!G.asDegrees(fingering).show.split(" ").contains("13"))
    assert(G.asDegrees(fingering).show.split(" ").contains("R"))
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
    assert(G5.semitones == List(0,7,0))
    val fingering = Chord.unapply("3 5 5 x x x")
    assert(G5.asDegrees(fingering).show == "R 5 R x x x")
  }

  it should "handle add9 chords" in {
    assert(Chord("Gadd9").semitones == List(0, 4, 7, 2))
    assert(Chord("G6add9").semitones == List(0, 4, 7, 9, 2))
    assert(Chord("Gm6add9").semitones == List(0, 3, 7, 9, 2))
    assert(Chord("Gadd9").asShell.semitones == List(0, 4, 2))
  }

  it should "handle add11 chords" in {
    assert(Chord("Gadd11").semitones == List(0, 4, 7, 5))
    assert(Chord("G6add11").semitones == List(0, 4, 7, 9, 5))
    assert(Chord("Gm6add11").semitones == List(0, 3, 7, 9, 5))
    assert(Chord("Gadd11").asShell.semitones == List(0, 4, 5))
  }

  it should "handle multiple adds" in {
    assert(Chord("Gadd9add11").semitones == List(0, 4, 7, 2, 5))
  }

  it should "handle banjo chords" in {
    implicit val tuning = Tuning("D G B D")
    val c = Chord("C")
    assert(c.semitones == List(0,4,7))
    val fingering = Chord.unapply("2 0 1 x")
    assert(c.asDegrees(fingering).show == "3 5 R x")
  }

  it should "handle frettings" in {
    implicit val tuning = Tuning.StandardTuning
    val result = List(Some(1), None, Some(2), Some(0), Some(1), None)
    assert(Chord.unapply(" 1 x 2 0 1 x") == result)
    assert(Chord.unapply("1  x 2 0 1 x") == result)
    assert(Chord.unapply("1 x 2 0 1 x ") == result)
    assert(Chord.unapply("1x201x") == result)
    assert(Chord.unapply("1X201x") == result)
  }

  it should "compute diffs" in {
    implicit val tuning = Tuning.StandardTuning
    val em = Chord.unapply("0 x 0 0 x 7")
    val c7 = Chord.unapply("0 3 x 0 0 x")
    val c = diff(em, c7)
    println(c)
  }

  it should "handle suspensions" in {
    assert(Chord("Asus4").semitones == List(0, 5, 7))
    assert(Chord("Asus2").semitones == List(0, 2, 7))
  }

  it should "handle #11 chords" in {
    assert(Chord("C7#11").semitones == List(0, 4, 7, 10, 6))
  }

  it should "handle b13 chords" in {
    assert(Chord("C7b13").semitones == List(0, 4, 7, 10, 8))
  }

  it should "handle random chords" in {
    forAll(ChordGenerator.chordGen) {(c) =>
      println(s"c: $c")
      Chord(c).semitones shouldBe List(0, 4, 7)
    }
  }
}
