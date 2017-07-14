package chord

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by ericolander on 7/13/17.
  */
class ScaleSpec extends FlatSpec with Matchers {

  "Scale" should "" in {

  }

  it should "handle major scales" in {
    val cMaj = MajorScale(Note("C"))
    cMaj.relativeMinor shouldBe Aeolian(Note("A"))
    cMaj.root shouldBe Note("C")
    cMaj.relativeMinor.root shouldBe Note("A")
  }

}
