package chord

/**
 * Created by eolander on 12/28/14.
 */
class ShellChord(chord: Chord)
extends Chord(chord.root, chord.triad, chord.quality, chord.extension, chord.alteration, chord.added,
              chord.suspension, chord.altRoot) {

  override def intervals(extensions: => List[String] = List(extension.toString)): List[String] = {
    chord.intervals(extensions).filterNot(_.contains("5"))
  }

  //override def extensions = {println("shell"); List(extension.toString)}
}

object ShellChord {
  def apply(c: Chord): Chord = new ShellChord(c)
}
