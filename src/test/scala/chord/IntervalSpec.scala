package chord

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntervalSpec extends AnyFlatSpec with Matchers {

  "An interval" should "be invertable" in {
    Interval("m3").invert shouldBe Interval("M6")
    Interval("P5").invert shouldBe Interval("P4")
    Interval("m2").invert shouldBe Interval("M7")
    Interval("m7").invert shouldBe Interval("M2")
  }

}
