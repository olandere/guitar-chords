package chord

import org.scalatest._

/**
  * Created by eolander on 4/23/16.
  */
class ChordParserSpec extends FlatSpec with ShouldMatchers {

  "ChordParser" should "recognize power chords" in {
    ChordParser("A5")
  }

  it should "recognize lists of chords" in {
    ChordParser("E A B5 Dm7")
  }
}
