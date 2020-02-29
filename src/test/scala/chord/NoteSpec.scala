package chord

import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/**
  * Created by ericolander on 5/13/17.
  */
class NoteSpec extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000)

  it should "create notes" in {
    forAll(NoteGenerator.noteGen) { (n) =>
  //    println(s"note: $n")
    }
  }

  it should "correctly handle enharmonic names" in {
    forAll(NoteGenerator.noteGen.filter(n => !Set("C♭", "B♯", "F♭", "E♯").contains(n.toString))) { (n) =>
     // println(s"note: $n")
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
    Sharp.adjust(Note("C")) shouldBe Note("C#")
    Sharp.adjust(Note("C#")) shouldBe Note("C" + DOUBLE_SHARP)
    Sharp.adjust(Note("Db")) shouldBe Note("D")
    Sharp.adjust(Note("D" + DOUBLE_FLAT)) shouldBe Note("Db")
  }

  it should "adjust accidentals for sharps" in {
    Sharp.adjust(Natural) shouldBe Natural
    Sharp.adjust(DoubleSharp) shouldBe Sharp
    Sharp.adjust(Sharp) shouldBe Natural
  }

  it should "handle flats" in {
    Flat.adjust(Note("D")) shouldBe Note("Db")
    Flat.adjust(Note("D#")) shouldBe Note("D")
    Flat.adjust(Note("D" + DOUBLE_SHARP)) shouldBe Note("D#")
    Flat.adjust(Note("Db")) shouldBe Note("D" + DOUBLE_FLAT)
  }

  it should "handle double sharps" in {
    Note("A" + DOUBLE_SHARP).enharmonic shouldBe Note("B")
    Note("B" + DOUBLE_SHARP).enharmonic shouldBe Note("C#")
    DoubleSharp.adjust(Note("C")) shouldBe Note("C" + DOUBLE_SHARP)
    DoubleSharp.adjust(Note("Ab")) shouldBe Note("A#")
    DoubleSharp.adjust(Note("D" + DOUBLE_FLAT)) shouldBe Note("D")
    Note("A" + DOUBLE_SHARP).toString shouldBe "A" + DOUBLE_SHARP
  }

  it should "handle double flats" in {
    Note("A" + DOUBLE_FLAT).enharmonic shouldBe Note("G")
    Note("C" + DOUBLE_FLAT).enharmonic shouldBe Note("Bb")
    DoubleFlat.adjust(Note("D")) shouldBe Note("D" + DOUBLE_FLAT)
    DoubleFlat.adjust(Note("G#")) shouldBe Note("Gb")
    DoubleFlat.adjust(Note("A" + DOUBLE_SHARP)) shouldBe Note("A")
  }

  it should "handle degrees" in {
    Degree("b3").toString shouldBe "♭3"
    Degree("#4").semitone shouldBe 6
    Degree("3").semitone shouldBe 4
  }

  it should "handle extended degrees" in {
    Degree("#11").semitone shouldBe 6
  }

  it should "adjust notes for altered degrees" in {
    val e = Note("E")
    val flat3 = Degree("b3")
    flat3.adjust(e) shouldBe Note("Eb")
  }

  it should "adjust notes in scales" in {
    val sd = Degree("b6")
    val c = Note("C")
    val d = Degree("6")
    sd.adjust(d).adjust(c) shouldBe Note("C#")
  }

  it should "not care about case differences" in {
    Note("c") shouldBe Note("C")
  }

  it should "account for accidentals" in {
    Note("C#") shouldNot be(Note("C"))
  }

  it should "handle enharmonic equivalence" in {
    Note("B").enhEquals(Note("Cb")) shouldBe true
    Note("A"+DOUBLE_SHARP).enhEquals(Note("B")) shouldBe true
  }

  it should "successfully compute intervals between notes" in {
    forAll(NoteGenerator.noteGen, NoteGenerator.noteGen) { (n1, n2) =>
      n1.interval(n2)
    }
  }

  it should "successfully raise notes by an interval between notes" in {
    forAll(NoteGenerator.noteGen, NoteGenerator.noteGen) { (n1, n2) =>
      n1.raise(n1.interval(n2)) shouldBe n2
    }
  }

  it should "successfully lower notes by an interval between notes" in {
    forAll(NoteGenerator.noteGen, NoteGenerator.noteGen) { (n1, n2) =>
      n1.lower(n2.interval(n1)) shouldBe n2
    }
  }

  "degrees" should "correctly convert to intervals" in {
    "1234567".map(c => Degree(c.toString).toInterval) shouldBe "P1 M2 M3 P4 P5 M6 M7".split(" ").map(i => Interval(i)).toList
  }

  "accidentals" should "be ordered" in {
    Natural < Sharp shouldBe true
    Flat < Natural shouldBe true
  }
}
