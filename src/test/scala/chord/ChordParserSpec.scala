package chord

import org.scalatest._

/**
  * Created by eolander on 4/23/16.
  */
class ChordParserSpec extends FlatSpec with Matchers {
  import chord._

  "ChordParser" should "recognize power chords" in {
    ChordParser("A5") shouldBe List(PowerChord("A"))
  }

  it should "recognize lists of chords" in {
    ChordParser("E A B5 Dm7") shouldBe List(Chord("E"), Chord("A"), PowerChord("B"), Chord("Dm7"))
    ChordParser("C/G,G,D/A,A5") shouldBe List(Chord("C/G"), Chord("G"), Chord("D/A"), PowerChord("A"))
  }

  it should "handle slash chords" in {
    ChordParser("D/A") shouldBe List(Chord("D/A"))
  }

  it should "handle multiple adds" in {
    ChordParser("Gadd9add11") should be (List(Chord("Gadd9add11")))
  }

  it should "handle strange chords" in {
    ChordParser("G♭5♭9") should be (List(Chord("G♭5♭9"))) //5♭9no3
  }

  it should "handle integer notation" in {
    ChordParser("{0 1 4 6 8}").head.toString shouldBe "{0 1 4 6 8}"
  }

  it should "handle integer notation with root" in {
    ChordParser("A {0 1 4 6 8}").head.toString shouldBe "A{0 1 4 6 8}"
  }
}
