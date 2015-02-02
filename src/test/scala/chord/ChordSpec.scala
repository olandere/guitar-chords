package chord

import org.scalatest._

/**
 * Created by eolander on 1/4/15.
 */
class ChordSpec extends FlatSpec with ShouldMatchers {

  "A chord" should "have correct semitones" in {
    assert(Chord("Gdim7").semitones == List(0,3,6,9))
    assert(Chord("Cdim").semitones == List(0,3,6))
    assert(Chord("Eb").semitones == List(0,4,7))
    assert(Chord("Bb-7").semitones == List(0,3,7,10))
    assert(Chord("F#-7b5").semitones == List(0,3,6,10))
    assert(Chord("F9").semitones == List(0,4,7,10,2))
    assert(Chord("AM13").semitones == List(0,4,7,11,2,5,9))
    assert(Chord("C6").semitones == List(0,4,7,9))
    assert(Chord("Db+").semitones == List(0,4,8))
    assert(Chord("B+11").semitones == List(0,4,8,10,2,5))
    assert(Chord("E7b9").semitones == List(0,4,7,10,1))
  }

  it should "be a correct shell chord" in {
    assert(Chord("Bb-7").asShell.semitones == List(0,3,10))
    assert(Chord("AM13").asShell.semitones == List(0,4,11,9))
  }

}
