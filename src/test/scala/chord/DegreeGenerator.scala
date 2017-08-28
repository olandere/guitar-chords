package chord

import org.scalacheck.Gen

object DegreeGenerator {
  val dGen: Gen[Char] = Gen.oneOf("234567")

  val accidentalGen: Gen[Char] = Gen.frequency(
    (2, ' '),
    (1, 'b'),
    (1, '#')
  )

  val degreeGen: Gen[Degree] = for {
    r <- dGen
    a <- accidentalGen
  } yield Degree((a + r.toString).trim())
}
