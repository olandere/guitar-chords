package chord

import CircleOfFifths._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CircleOfFifthsSpec extends AnyFlatSpec with Matchers {

  "CircleOfFifths" should "find relative minor key" in {
    relativeMinor("C") shouldBe "A"
    relativeMinor("G") shouldBe "E"
    relativeMinor("F") shouldBe "D"
  }

  it should "determine the number of sharps" in {
    numberOfSharps("C") shouldBe 0
    numberOfSharps("D") shouldBe 2
  }

  it should "generate major scales" in {
    majorScale(Note("G")).mkString shouldBe "GABCDEF♯"
    majorScale(Note("D")).mkString shouldBe "DEF♯GABC♯"
    majorScale(Note("C")).mkString shouldBe "CDEFGAB"
    majorScale(Note("G#")).mkString shouldBe s"G♯A♯B♯C♯D♯E♯F$DOUBLE_SHARP"
  }

  it should "generate minor scales" in {
    minorScale(Note("G")).mkString shouldBe "GAB♭CDE♭F"
    minorScale(Note("D")).mkString shouldBe "DEFGAB♭C"
    minorScale(Note("C")).mkString shouldBe "CDE♭FGA♭B♭"
    minorScale(Note("G#")).mkString shouldBe "G♯A♯BC♯D♯EF♯"
    minorScale(Note("Db")).mkString shouldBe minorScale(Note("C#")).mkString
  }

  it should "give correct enharmonic names" in {
    enharmonic(Note("B#")) shouldBe Note("C")
    enharmonic(Note("Cb")) shouldBe Note("B")
    enharmonic(Note("A#")) shouldBe Note("B♭")
    enharmonic(Note("Bb")) shouldBe Note("A#")
  }
}