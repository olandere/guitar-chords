package chord

import org.scalatest._
import CircleOfFifths._

class CircleOfFifthsSpec extends FlatSpec {

  "CircleOfFifths" should "find relative minor key" in {
    assert(relativeMinor("C") == "A")
    assert(relativeMinor("G") == "E")
    assert(relativeMinor("F") == "D")
  }

  it should "determine the number of sharps" in {
    assert(numberOfSharps("C") == 0)
    assert(numberOfSharps("D") == 2)
  }

  it should "generate major scales" in {
    assert(majorScale(Note("G")).mkString == "GABCDEF#")
    assert(majorScale(Note("D")).mkString == "DEF#GABC#")
    assert(majorScale(Note("C")).mkString == "CDEFGAB")
    assert(majorScale(Note("G#")).mkString == majorScale(Note("Ab")).mkString)
  }

  it should "generate minor scales" in {
    assert(minorScale(Note("G")).mkString == "GABbCDEbF")
    assert(minorScale(Note("D")).mkString == "DEFGABbC")
    assert(minorScale(Note("C")).mkString == "CDEbFGAbBb")
    assert(minorScale(Note("G#")).mkString == "G#A#BC#D#EF#")
    assert(minorScale(Note("Db")).mkString == minorScale(Note("C#")).mkString)
  }

  it should "give correct enharmonic names" in {
    assert(enharmonic(Note("B#")) == Note("C"))
    assert(enharmonic(Note("Cb")) == Note("B"))
    assert(enharmonic(Note("A#")) == Note("B♭"))
    assert(enharmonic(Note("Bb")) == Note("A#"))
  }
}