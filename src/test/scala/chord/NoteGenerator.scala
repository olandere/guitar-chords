package chord

import org.scalacheck.Gen

/**
  * Created by ericolander on 12/28/16.
  */
object NoteGenerator {

  val nGen: Gen[Char] = Gen.oneOf("ABCDEFG")

  val accidentalGen: Gen[Char] = Gen.frequency(
    (2, ' '),
    (1, 'b'),
    (1, '#')
  )

  val allSccidentalGen: Gen[String] = Gen.frequency(
    (1, "b"),
    (1, "#"),
    (1, DOUBLE_FLAT),
    (1, DOUBLE_SHARP)
  )

  val noteWithAccidentalGen: Gen[Note] = for {
    r <- nGen
    a <- allSccidentalGen
  } yield Note((r.toString + a).trim())

  val noteGen: Gen[Note] = for {
    r <- nGen
    a <- accidentalGen
  } yield Note((r.toString + a).trim())
}
