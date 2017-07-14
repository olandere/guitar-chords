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
    forAll(NoteGenerator.noteGen) { (n) =>
      println(s"note: $n")
    }
  }

  it should "correctly handle enharmonic names" in {
    forAll(NoteGenerator.noteGen.filter(n => !Set("C♭", "B♯", "F♭", "E♯").contains(n.toString))) { (n) =>
      println(s"note: $n")
      n.enharmonic.enharmonic shouldBe n
    }
  }

  it should "handle specific enharmonics" in {
    Note("Cb").enharmonic shouldBe Note("B")
    Note("B#").enharmonic shouldBe Note("C")
    Note("Bb").enharmonic shouldBe Note("A#")
    Note("C#").enharmonic shouldBe Note("Db")
  }

  it should "handle sharps" in {
    Sharp().adjust(Note("C")) shouldBe Note("C#")
    Sharp().adjust(Note("C#")) shouldBe Note("C" + DOUBLE_SHARP)
    Sharp().adjust(Note("Db")) shouldBe Note("D")
    Sharp().adjust(Note("D" + DOUBLE_FLAT)) shouldBe Note("Db")
  }

  it should "adjust accidentals for sharps" in {
    Sharp().adjust(None) shouldBe None
    Sharp().adjust(Some(DoubleSharp())) shouldBe Some(Sharp())
    Sharp().adjust(Some(Sharp())) shouldBe Some(Natural())
  }

  it should "handle flats" in {
    Flat().adjust(Note("D")) shouldBe Note("Db")
    Flat().adjust(Note("D#")) shouldBe Note("D")
    Flat().adjust(Note("D" + DOUBLE_SHARP)) shouldBe Note("D#")
    Flat().adjust(Note("Db")) shouldBe Note("D" + DOUBLE_FLAT)
  }

  it should "handle double sharps" in {
    Note("A" + DOUBLE_SHARP).enharmonic shouldBe Note("B")
    Note("B" + DOUBLE_SHARP).enharmonic shouldBe Note("C#")
    DoubleSharp().adjust(Note("C")) shouldBe Note("C" + DOUBLE_SHARP)
    DoubleSharp().adjust(Note("Ab")) shouldBe Note("A#")
    DoubleSharp().adjust(Note("D" + DOUBLE_FLAT)) shouldBe Note("D")
    Note("A" + DOUBLE_SHARP).toString shouldBe "A" + DOUBLE_SHARP
  }

  it should "handle double flats" in {
    Note("A" + DOUBLE_FLAT).enharmonic shouldBe Note("G")
    Note("C" + DOUBLE_FLAT).enharmonic shouldBe Note("Bb")
    DoubleFlat().adjust(Note("D")) shouldBe Note("D" + DOUBLE_FLAT)
    DoubleFlat().adjust(Note("G#")) shouldBe Note("Gb")
    DoubleFlat().adjust(Note("A" + DOUBLE_SHARP)) shouldBe Note("A")
  }

  it should "handle degrees" in {
    Degree("b3").toString shouldBe "♭3"
  }

  it should "adjust notes for altered degrees" in {
    val e = Note("E")
    val flat3 = Degree("b3")
    flat3.adjust(e) shouldBe Note("Eb")
  }
}
