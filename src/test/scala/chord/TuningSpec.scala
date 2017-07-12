package chord

import org.scalatest._

/**
 * Created by eolander on 2/25/15.
 */
class TuningSpec extends FlatSpec with Matchers {

  "A tuning" should "not case about case" in {
    val t = Tuning("a b c d e")
    t.root shouldBe "A"
    t.numStrings shouldBe 5
    t.semitones shouldBe List(0,2,3,5,7)
  }

  it should "handle sharps and flats" in {
    val t = Tuning("a b c# d e♭")
    t.root shouldBe "A"
    t.numStrings shouldBe 5
    t.semitones shouldBe List(0,2,4,5,6)
  }

  it should "not case about whitespace" in {
    val t = Tuning(" a   b c   d eb  ")
    t.root shouldBe "A"
    t.numStrings shouldBe 5
    t.semitones shouldBe List(0,2,3,5,6)
  }

  it should "print the tuning" in {
    val t = Tuning("C# G# C# G# C# E")
    t.toString shouldBe "C# G# C# G# C# E"
  }
}
