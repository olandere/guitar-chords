package chord

import org.scalatest.{FlatSpec, Matchers}
import Note._

/**
  * Created by ericolander on 7/13/17.
  */
class ScaleSpec extends FlatSpec with Matchers {

  "Scale" should "" in {

  }

  it should "handle major scales" in {
    val cMaj = MajorScale(Note("C"))
    cMaj.relativeMinor shouldBe Aeolian(Note("A"))
    cMaj.root shouldBe Note("C")
    cMaj.relativeMinor.root shouldBe Note("A")
    cMaj.notes shouldBe notes("C,D,E,F,G,A,B")
    cMaj.relativeMinor.notes shouldBe notes("A,B,C,D,E,F,G")
  }

  it should "handle sharp keys" in {
    val dMaj = MajorScale(Note("D"))
    dMaj.relativeMinor shouldBe Aeolian(Note("B"))
    dMaj.notes shouldBe notes("D,E,F#,G,A,B,C#")
  }

  it should "handle flat keys" in {
    val fMaj = MajorScale(Note("F"))
    fMaj.relativeMinor shouldBe Aeolian(Note("D"))
    fMaj.notes shouldBe notes("F,G,A,Bb,C,D,E")
  }

  it should "handle F# and C# major" in {

  }

  it should "handle Gb and Cb major" in {

  }

  it should "handle modes" in {
    val cMaj = MajorScale(Note("C"))
    Dorian(Note("D")).relatedScale shouldBe cMaj
    Phrygian(Note("E")).relatedScale shouldBe cMaj
    Lydian(Note("F")).relatedScale shouldBe cMaj
    Mixolydian(Note("G")).relatedScale shouldBe cMaj
    Aeolian(Note("A")).relatedScale shouldBe cMaj
    Locrian(Note("B")).relatedScale shouldBe cMaj
    SuperLocrian(Note("B")).relatedScale shouldBe cMaj
  }

}
