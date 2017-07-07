package chord

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * Created by ericolander on 5/13/17.
  */
class NoteSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50)

  it should "create notes" in {
    forAll(NoteGenerator.noteGen) {(n) =>
      println(s"note: $n")
    }
  }

  it should "correctly handle enharmonic names" in {
    forAll(NoteGenerator.noteGen.filter(n => !Set("C♭", "B♯", "F♭", "E♯").contains(n.toString))) {(n) =>
      println(s"note: $n")
      assert(n.enharmonic.enharmonic == n)
    }
  }

  it should "handle specific enharmonics" in {
    assert(Note("Cb").enharmonic == Note("B") )
    assert(Note("B#").enharmonic == Note("C") )
    assert(Note("Bb").enharmonic == Note("A#") )
    assert(Note("C#").enharmonic == Note("Db") )
  }

  it should "handle double sharps" in {
    assert(Note("A"+DOUBLE_SHARP).enharmonic == Note("B"))
    assert(Note("B"+DOUBLE_SHARP).enharmonic == Note("C#"))
  }

  it should "handle double flats" in {
    assert(Note("A"+DOUBLE_FLAT).enharmonic == Note("G"))
    assert(Note("C"+DOUBLE_FLAT).enharmonic == Note("Bb"))
  }

  it should "handle degrees" in {
    assert(Degree("b3").toString == "♭3")
  }

  it should "adjust notes for altered degrees" in {
    val e = Note("E")
    val flat3 = Degree("b3")
    assert(flat3.adjust(e) == Note("Eb"))
  }
}
