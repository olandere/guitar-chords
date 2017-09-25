package chord

/**
 * Created by eolander on 12/28/14.
 */
class ShellChord(chord: Chord)
extends Chord(chord.root, chord.triad, chord.quality, chord.extension, chord.alteration, chord.added,
              chord.suspension, chord.altRoot) {

  override def intervals(extensions: => List[Degree] = List(Degree(extension, Natural))): List[Degree] = {
    chord.intervals(extensions).filterNot(_ == Degree("5"))
  }

  //override def extensions = {println("shell"); List(extension.toString)}
}

object ShellChord {
  def apply(c: Chord): Chord = new ShellChord(c)
}
