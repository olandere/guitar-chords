package chord

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Created by eolander on 4/23/16.
  */
class TuningParserSpec extends AnyFlatSpec with Matchers {

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

  it should "handle invalid input" in {
    TuningParser("asfd") shouldBe Tuning.StandardTuning
  }
}
