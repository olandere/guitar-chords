package chord

import org.scalatest._
import Operations._

/**
 * Created by eolander on 2/17/15.
 */
class ChordNamerSpec extends FlatSpec with ShouldMatchers {

  "ChordNamer" should "identify chords" in {
    //assert(ChordNamer(chords("1 3 3 1 1 1")) == "m")
    //assert(ChordNamer(List(0,7,10,3,7)) == "m7")

  }

  it should "determine inversions" in {
    assert(ChordNamer("1 3 3 1 1 1").determineInversion == "root")
    assert(ChordNamer("x 0 2 2 2 0").determineInversion == "root")
    assert(ChordNamer("x 3 2 2 1 0").determineInversion == "1st")
    assert(ChordNamer("0 0 2 2 2 0").determineInversion == "2nd")

    assert(ChordNamer("5 x 5 5 5 x").determineInversion == "root")
    assert(ChordNamer("5 x 4 5 5 x").determineInversion == "1st")
    assert(ChordNamer("5 x 6 6 5 x").determineInversion == "root")
    assert(ChordNamer("x 3 5 4 5 x").determineInversion == "root")
    //assert(ChordNamer("x 3 4 2 4 x").determineInversion == "dim7")
  }

  it should "handle extensions" in {
    assert(ChordNamer("x 7 6 7 7 x").determineInversion == "root")
  }

  it should "respell" in {
    assert(ChordNamer("x 3 2 2 1 0").respell.determineInversion == "root")
    assert(ChordNamer("0 0 2 2 2 0").respell.determineInversion == "root")
    ChordNamer("5 x 4 5 5 x").respell
  }

  it should "detect quality" in {
    assert(ChordNamer("x 3 2 2 1 0").respell.quality == "m")
  }

  it should "name chords" in {
    assert(ChordNamer("x 3 2 2 1 0").respell.toString == "Am")
    assert(ChordNamer("x 0 2 2 2 0").respell.toString == "A")
    assert(ChordNamer("x 0 2 0 2 0").respell.toString == "A7")
    assert(ChordNamer("x 0 2 0 1 0").respell.toString == "Am7")
    assert(ChordNamer("5 x 5 5 5 x").respell.toString == "Am7")
    assert(ChordNamer("5 x 6 6 5 x").respell.toString == "AM7")
    assert(ChordNamer("5 x 5 6 6 x").respell.toString == "A+7")
    assert(ChordNamer("7 x 7 7 6 x").respell.toString == "Bm7♭5")
    assert(ChordNamer("x 7 6 7 6 x").respell.toString == "E7♭9")
    assert(ChordNamer("5 x 5 6 7 x").respell.toString == "A13")
    assert(ChordNamer("x 0 8 6 8 0").respell.toString == "A7♭9")
    assert(ChordNamer("x x 0 2 3 0").respell.toString == "Dsus2")
    assert(ChordNamer("x 0 2 0 3 0").respell.toString == "A7sus4")
  }

}
