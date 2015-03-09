package chord

import org.scalatest._

/**
 * Created by eolander on 2/25/15.
 */
class TuningSpec extends FlatSpec with ShouldMatchers {

  "A tuning" should "not case about case" in {
    val t = Tuning("a b c d e")
    assert(t.root == "A")
    assert(t.numStrings == 5)
    assert(t.semitones == List(0,2,3,5,7))
  }

  it should "handle sharps and flats" in {
    val t = Tuning("a b c# d e♭")
    assert(t.root == "A")
    assert(t.numStrings == 5)
    assert(t.semitones == List(0,2,4,5,6))
  }

  it should "not case about whitespace" in {
    val t = Tuning(" a   b c   d eb  ")
    assert(t.root == "A")
    assert(t.numStrings == 5)
    assert(t.semitones == List(0,2,3,5,6))
  }
}