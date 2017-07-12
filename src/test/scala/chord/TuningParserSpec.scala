package chord

import org.scalatest._

/**
  * Created by eolander on 4/23/16.
  */
class TuningParserSpec extends FlatSpec with Matchers {

  "The tuning parser" should "not care about spacing" in {
    TuningParser("D A D G A D") shouldBe Tuning.DADGAD
    TuningParser("DADGAD") shouldBe Tuning.DADGAD
  }

  it should "not care about caseing" in {
    TuningParser("dadgad") shouldBe Tuning.DADGAD
  }

  it should "understand Joni Mitchell notation" in {
    TuningParser("E55545") shouldBe Tuning.StandardTuning
  }
}
