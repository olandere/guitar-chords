package chord

import org.scalatest._

/**
  * Created by eolander on 4/23/16.
  */
class TuningParserSpec extends FlatSpec {

  "The tuning parser" should "not care about spacing" in {
    assert(TuningParser("D A D G A D") == Tuning.DADGAD)
    assert(TuningParser("DADGAD") == Tuning.DADGAD)
  }

  it should "not care about caseing" in {
    assert(TuningParser("dadgad") == Tuning.DADGAD)
  }

  it should "understand Joni Mitchell notation" in {
    assert(TuningParser("E55545") == Tuning.StandardTuning)
  }
}
