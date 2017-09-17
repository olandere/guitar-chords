package chord

import org.scalacheck.Gen

object ScaleGenerator {

  val scaleGen: Gen[Scale] = for {
    r <- NoteGenerator.noteGen
    s <- Gen.oneOf(Scale.allScales(r))
  } yield s
}
