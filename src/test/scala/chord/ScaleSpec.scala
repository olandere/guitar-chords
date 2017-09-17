package chord

import org.scalatest.{FlatSpec, Matchers}
import Note._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * Created by ericolander on 7/13/17.
  */
class ScaleSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50)

  "Scale" should "" in {

  }

  it should "handle major scales" in {
    val cMaj = Major(Note("C"))
    cMaj.relativeMinor shouldBe Aeolian(Note("A"))
    cMaj.root shouldBe Note("C")
    cMaj.relativeMinor.root shouldBe Note("A")
    cMaj.notes shouldBe notes("C,D,E,F,G,A,B")
    cMaj.relativeMinor.notes shouldBe notes("A,B,C,D,E,F,G")
  }

  it should "handle sharp keys" in {
    val dMaj = Major(Note("D"))
    dMaj.relativeMinor shouldBe Aeolian(Note("B"))
    dMaj.notes shouldBe notes("D,E,F#,G,A,B,C#")
  }

  it should "handle flat keys" in {
    val fMaj = Major(Note("F"))
    fMaj.relativeMinor shouldBe Aeolian(Note("D"))
    fMaj.notes shouldBe notes("F,G,A,Bb,C,D,E")
  }

  it should "handle F# and C# major" in {
    Major(Note("C#")).notes shouldBe notes("C#,D#,E#,F#,G#,A#,B#")
    Major(Note("F#")).notes shouldBe notes("F#,G#,A#,B,C#,D#,E#")
  }

  it should "handle Gb and Cb major" in {
    Major(Note("Gb")).notes shouldBe notes("Gb,Ab,Bb,Cb,Db,Eb,F")
    Major(Note("Cb")).notes shouldBe notes("Cb,Db,Eb,Fb,Gb,Ab,Bb")
  }

  it should "handle modes" in {
    val cMaj = Major(Note("C"))
    Dorian(Note("D")).relatedScale shouldBe cMaj
    Phrygian(Note("E")).relatedScale shouldBe cMaj
    Lydian(Note("F")).relatedScale shouldBe cMaj
    Mixolydian(Note("G")).relatedScale shouldBe cMaj
    Aeolian(Note("A")).relatedScale shouldBe cMaj
    Locrian(Note("B")).relatedScale shouldBe cMaj
    SuperLocrian(Note("B")).relatedScale shouldBe cMaj
  }

  it should "create scales from names" in {
    Scale(Note("C"), "Dorian") shouldBe Dorian(Note("C"))
  }

  it should "create a scale from degrees" in {
    Scale(Note("C"), "2 3 5 6").semitones shouldBe MajorPentatonic(Note("C")).semitones
  }

  it should "compute the name of a scale" in {
    MelodicMinor(Note("A")).toString shouldBe "A Melodic Minor"
  }

  it should "get names of all supported scales" in {
    Scale.supportedScales should contain("Major")
    Scale.supportedScales should contain("Super Locrian")
    Scale.supportedScales shouldNot contain("Scale By Degrees")
  }

  it should "get all scales for a note" in {
    Scale.allScales(Note("C")) should contain(Locrian(Note("C")))
  }

  it should "validate random scales have the correct number of notes" in {
    forAll(ScaleGenerator.scaleGen) { (s) =>
      val noteSet = s.notes.map(_.name).toSet
      if (s.name.contains("Pentatonic"))
        noteSet.size shouldBe 5
      else
        noteSet.size shouldBe 7
    }
  }

}
