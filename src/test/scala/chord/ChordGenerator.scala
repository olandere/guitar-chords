package chord

import org.scalacheck.Gen

/**
  * Created by ericolander on 12/28/16.
  */
object ChordGenerator {

  val rootGen = Gen.oneOf("ABCDEFG")
  val accidentalGen = Gen.frequency(
    (2, ' '),
    (1, 'b'),
    (1, '#')
  )

  val chordGen = for {
    r <- rootGen
    a <- accidentalGen
  } yield (r.toString + a).trim()
}
