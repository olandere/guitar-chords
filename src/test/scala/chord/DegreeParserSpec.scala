package chord

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class DegreeParserSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 50)

  "DegreeParser" should "parse Degree objects" in {
    forAll(DegreeGenerator.degreeGen) { (n) =>
      DegreeParser(n.toString).head shouldBe n
    }
  }

  it should "handle multiple degrees" in {
    DegreeParser("R 2 b3 4 5 6 7").map(_.semitone) shouldBe MelodicMinor(Note("D")).intervals
  }

}
