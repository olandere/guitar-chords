package chord

import org.scalatest._

class CircleOfFifthsSpec extends FlatSpec {

	"CircleOfFifths" should "find relative minor key" in {
        assert(CircleOfFifths.relativeMinor("C") == "A")
        assert(CircleOfFifths.relativeMinor("G") == "E")
        assert(CircleOfFifths.relativeMinor("F") == "D")
	}

	it should "determine the number of sharps" in {
      assert(CircleOfFifths.numberOfSharps("C") == 0)
      assert(CircleOfFifths.numberOfSharps("D") == 2)
	}

	it should "generate major scales" in {
		assert(CircleOfFifths.majorScale("G").mkString == "GABCDEF#")
		assert(CircleOfFifths.majorScale("D").mkString == "DEF#GABC#")
		assert(CircleOfFifths.majorScale("C").mkString == "CDEFGAB")
	}
}
