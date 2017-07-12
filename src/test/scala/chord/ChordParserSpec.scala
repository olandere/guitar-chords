package chord

import org.scalatest._

/**
  * Created by eolander on 4/23/16.
  */
class ChordParserSpec extends FlatSpec with Matchers {

  "ChordParser" should "recognize power chords" in {
    ChordParser("A5") shouldBe List(new PowerChord("A"))
  }

  it should "recognize lists of chords" in {
    ChordParser("E A B5 Dm7") shouldBe List(Chord("E"), Chord("A"), new PowerChord("B"), Chord("Dm7"))
    ChordParser("C/G,G,D/A,A5") shouldBe List(Chord("C/G"), Chord("G"), Chord("D/A"), new PowerChord("A"))
  }

  it should "handle slash chords" in {
    ChordParser("D/A") shouldBe List(Chord("D/A"))
  }

  it should "handle multiple adds" in {
    ChordParser("Gadd9add11") should be (List(Chord("Gadd9add11")))
  }
}
