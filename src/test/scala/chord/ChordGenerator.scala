package chord

import org.scalacheck.Gen

/**
  * Created by ericolander on 12/28/16.
  */
object ChordGenerator {

  val triadGen: Gen[String] = Gen.oneOf("m", "-", "+", "aug", "dim", "°", "")

  val chordNameGen: Gen[String] = for {
    r <- NoteGenerator.noteGen
    t <- triadGen
  } yield List(r.toString, t).mkString //, Operations.fingerings(Chord(r + a + t)))

  val chordGen: Gen[Chord] = for {
    c <- chordNameGen
  } yield Chord(c)
}
