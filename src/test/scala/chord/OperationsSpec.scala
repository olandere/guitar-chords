package chord

import cats.implicits._
import chord.Operations._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Created by eolander on 12/28/14.
 */
class OperationsSpec extends AnyFlatSpec with Matchers with Inspectors {

  "Fingerings" should "generate known fingerings" in
    {

    fingerings(Chord("E-9").asShell, 3).map{_.show} should contain("x 7 5 7 7 x")
    fingerings(Chord("D-9").asShell, 3).map{_.show} should contain("x 5 3 5 5 x")
    fingerings(Chord("FM7"), 4).map{_.show} should contain("x x 3 2 1 0")
    fingerings(Chord("AM7"), 4).map{_.show} should contain("x x 7 6 5 4")
    fingerings(Chord("BM7"), 4).map{_.show} should contain("7 x 8 8 7 x")
    fingerings(Chord("A"), 3).map{_.show} should contain("0 x x 6 x 5")

    assert(fingerings(Chord("E"), 4).forall(c => c.forall(_.forall(_>=0))))
  }

  it should "handle banjo chords" in {
    implicit val tuning: Tuning = Tuning("D G B D")
    fingerings(Chord("A"), 4).map{_.show} should contain("x 2 2 2")
    fingerings(Chord("E7").asShell, 4).map{_.show} should contain("x 1 3 2")
    assert(fingerings(Chord("C"), 6, jazzVoicing = true).map{_.show}.forall(c=>c.exists(_=='x')))  //todo: should have exactly one 'x'
  }

  it should "handle slash chords" in {
    fingerings(Chord("A/D"), 4).map{_.show} should contain("x x 12 9 10 9")
  }

  it should "understand add 9 chords" in {
    fingerings(Chord("Cadd9"), 4).map{_.show} should contain("x 3 2 0 3 x")
    fingerings(Chord("Cmadd9"), 4).map{_.show} should contain("x 3 1 0 3 x")
    fingerings(Chord("C6add9"), 4).map{_.show} should contain("x 3 2 0 3 5")
  }

  it should "understand add 11 chords" in {
    fingerings(Chord("Cadd11"), 4).map{_.show} should contain("x 3 3 0 5 x")
    fingerings(Chord("Cmadd11"), 4).map{_.show} should contain("x 3 3 x 4 3")
    fingerings(Chord("C6add11"), 4).map{_.show} should contain("x 3 3 0 5 5")
  }

  it should "understand 13 chords" in {
    val c = Chord("C13")
    fingerings(c, 4).map{_.show} should contain("3 3 5 3 5 5")

    assert(fingerings(c, 4).map{f : FretList => c.asDegrees(f)}.forall(_.contains(Some(Degree("13")))))
  }

  it should "unfortunately, handle power chords" in {
    fingerings(Chord("A5"), 4).map{_.show} should contain("5 7 7 x x x")
  }

  it should "analyze fingerings" in {
    chords("1 3 3 2 1 1")
    chords("1 3 3 1 1 1")
    chords("x 0 2 0 1 0")
    chords("4 x 4 4 4 x")
  }

  it should "chart progressions" in {
    implicit val tuning: Tuning = Tuning.StandardTuning
    val p = progression(List(Chord("Em7"), Chord("CM7")), 3)
  //  println(p)
  }

  it should "handle root position filter" in {
    val aM7 = new Chord(Chord("AM7")) with RootPosition
    fingerings(aM7, 4).map{_.show} should contain("x x 7 6 5 4")
    fingerings(aM7, 4).map{_.show} should contain("5 x 6 6 5 x")
  }

  it should "handle root position filter with altered tuning" in {
    implicit val tuning: Tuning = Tuning.DADGAD
    val am7 = new Chord(Chord("Am7")) with RootPosition
    fingerings(am7, 2).map{_.show} should contain("x 0 x 0 3 2")
  }

  it should "handle drop2 position filter" in {
    val cM7 = new Chord(Chord("CM7")) with Drop2
    fingerings(cM7, 4).map{_.show} should contain("3 3 2 4 x x")
    fingerings(cM7, 4) should have size 12

    val c7 = new Chord(Chord("C7")) with Drop2
    fingerings(c7, 4) should have size 12
  }

  it should "have correct drop2 fingerings" in {
    val c7 = new Chord(Chord("C7")) with Drop2
    val voicing3to6 = Set("3 3 2 3 x x", "6 7 5 5 x x", "8 10 8 9 x x", "12 13 10 12 x x")
    val voicing2to5 = Set("x 10 10 9 11 x", "x 1 2 0 1 x", "x 3 5 3 5 x", "x 7 8 5 8 x")
    val voicing1to4 = Set("x x 5 5 5 6", "x x 8 9 8 8", "x x 10 12 11 12", "x x 2 3 1 3")
//    println(fingerings(c7, 4).map{_.show})
    fingerings(c7, 5).map{_.show}.toSet.intersect(voicing3to6) shouldBe voicing3to6
    fingerings(c7, 4).map{_.show}.toSet.intersect(voicing2to5) shouldBe voicing2to5
    fingerings(c7, 4).map{_.show}.toSet.intersect(voicing1to4) shouldBe voicing1to4
  }

  it should "handle drop2and4 position filter" in {
    //println("handle drop2and4 position filter")
    val dbM7 = new Chord(Chord("DbM7")) with Drop2and4
    fingerings(dbM7, 5).map{_.show} should contain("4 4 x 5 6 x")
   // println("DbM7")
    //fingerings(dbM7, 5).foreach{c=>println(c.show)}
    fingerings(dbM7, 5) should have size 8

    val c7 = new Chord(Chord("C7")) with Drop2and4
    fingerings(c7, 4) should have size 8
  //  println("C7")
   // fingerings(c7, 5).foreach{c=>println(c.show)}

    val cM7 = new Chord(Chord("CM7")) with Drop2and4
    fingerings(cM7, 5) should have size 9
   // println("CM7")
   // fingerings(cM7, 5).foreach{c=>println(c.show)}
  }

  it should "handle modes" in {

    val EmajScale = Major(Note("E"))
    val AmajScale = Major(Note("A"))
    val FsDorian = Dorian(Note("F#"))
    val GsPhrygian = Phrygian(Note("G#"))
    val ALydian = Lydian(Note("A"))
    val BMixolydian = Mixolydian(Note("B"))
    val CsAeolian = Aeolian(Note("C#"))
    val DsLocrian = Locrian(Note("D#"))
    fingering(EmajScale) should be(
      Array(List(0, 2, 4, 5, 7, 9, 11, 12, 14), List(0, 2, 4, 6, 7, 9, 11, 12, 14),
            List(1, 2, 4, 6, 7, 9, 11, 13, 14), List(1, 2, 4, 6, 8, 9, 11, 13, 14),
            List(0, 2, 4, 5, 7, 9, 10, 12, 14), List(0, 2, 4, 5, 7, 9, 11, 12, 14)))

    fingering(AmajScale) should be(
      Array(List(0, 2, 4, 5, 7, 9, 10, 12, 14), List(0, 2, 4, 5, 7, 9, 11, 12, 14),
            List(0, 2, 4, 6, 7, 9, 11, 12, 14), List(1, 2, 4, 6, 7, 9, 11, 13, 14),
            List(0, 2, 3, 5, 7, 9, 10, 12, 14, 15), List(0, 2, 4, 5, 7, 9, 10, 12, 14)))

    fingering(FsDorian).head should be(fingering(EmajScale).head)
    fingering(GsPhrygian).head should be(fingering(EmajScale).head)
    fingering(ALydian).head should be(fingering(EmajScale).head)
    fingering(BMixolydian).head should be(fingering(EmajScale).head)
    fingering(CsAeolian).head should be(fingering(EmajScale).head)
    fingering(DsLocrian).head should be(fingering(EmajScale).head)

    assert (EmajScale.containsChord(Chord("EM7")))
    assert(!EmajScale.containsChord(Chord("Em7")))
    assert(!EmajScale.containsChord(Chord("A7")))
    assert(EmajScale.containsChord(Chord("AM7")))
    assert(EmajScale.containsChord(Chord("B7")))
    assert(ALydian.containsChord(Chord("EM7")))
    assert(FsDorian.containsChord(Chord("B7")))
  }

  it should "find possible scales" in {
    possibleScales(Chord("E7b9")).foreach(s => println(s"$s, ${s.relatedScale}"))
    possibleScales(Chord("G♯m7")).foreach(s => println(s"$s, ${s.relatedScale}"))
    possibleScales(Chord("Abm7")).foreach(s => println(s"$s, ${s.relatedScale}"))
  }

  it should "correctly adjust octaves" in {
    adjustOctave2(Chord.unapply("x 7 8 17 8 x")) shouldEqual Chord.unapply("x 7 8 5 8 x")
    adjustOctave2(Chord.unapply("x 19 8 17 8 x")) shouldEqual Chord.unapply("x 7 8 5 8 x")
  }

  it should "generate barre chords" in {
    fingerings(Chord("E")).map{_.show} should contain("0 2 2 1 0 0")
    fingerings(Chord("Am")).map{_.show} should contain("x 0 2 2 1 0")
  }

  it should "name notes in chords" in {
    val E = Chord("E")
    val eNotes = notes(E)(_)
    fingerings(E).map{eNotes}.map{_.show} should contain("E B E G♯ B E")

    val DM9 = Chord("DM9")
    val dNotes = notes(DM9)(_)
    fingerings(DM9).map{dNotes}.map{_.show} should contain("E A D A C♯ F♯")

    val Cdim7 = Chord("Cdim7")
    val cNotes = notes(Cdim7)(_)
    fingerings(Cdim7).map{cNotes}.map{_.show} should contain("x C E♭ B\uD834\uDD2B x G♭")
  }

  it should "handle chords with more tones than strings" in {
    implicit val tuning: Tuning = Tuning("G D A E")
    val c = Chord("C13")
    assert(fingerings(c, 4).map{f : FretList => c.asDegrees(f)}.forall(_.contains(Some(Degree("13")))))
  }

  it should "handle power chords in altered tunings" in {
    implicit val tuning: Tuning = Tuning("G D A E")
    val a5 = Chord("A5")
    fingerings(a5, 4) should not be empty
    fingerings(a5, 4).map{_.show} should contain("2 2 0 0")
  }

  it should "handle root position chords with shell voicings" in {
    val aM7 = new ShellChord(Chord("AM9")) with RootPosition
    fingerings(aM7, 4).map{_.show} should contain("5 4 6 4 x x")
    fingerings(aM7, 4).map{_.show} should not contain "4 x 6 6 0 5"
  }

  it should "name notes from fingering" in {
    chords("6x776x") should matchPattern { case (_, "B♭M7", _) => }
    chords("097000") should matchPattern { case (_, "Emadd9add11", _) => }
    val (deg, name, _) = chords("x01x12")
    (deg.show, name) should matchPattern { case ("x R ♭5 x ♭3 °7", "Adim7") => }
    chords("xx1212") should matchPattern { case (_, "D♯dim7", List(None, None, Some(Note('D', Sharp)),
    Some(Note('A', Natural)),
    Some(Note('C', Natural)), Some(Note('F', Sharp)))) => }
   // val noteList = "E,A,C#,G,B,E".split(",").map(Note.apply).toList
    chords("0 12 11 0 0 0") should matchPattern {
      case (_, _, List(
      Some(Note('E', Natural)),
      Some(Note('A', Natural)),
      Some(Note('C', Sharp)),
      Some(Note('G', Natural)),
      Some(Note('B', Natural)),
      Some(Note('E', Natural)),
      )) =>
    }
  }

  it should "name notes from fingering with altered tuning" in {
    implicit val tuning: Tuning = Tuning.DADGAD
    val (deg, name, notes) = chords("7755xx")
    (deg.show, name, notes.show) should matchPattern { case ("R 5 ♭7 ♭3 x x", "Am7", "A E G C x x") => }
  }


  it should "correctly handle InvalidChord" in {
    fingerings(InvalidChord, 4) shouldBe Nil
    arpeggio(InvalidChord) shouldBe Nil
    roots(InvalidChord.root) shouldBe Nil
  }

  it should "generate valid fret positions for arpeggios" in {
    forAll (arpeggio(Chord("Ab"))) { fl =>
      forAll (fl) { f => f should be >= 0 }
    }
  }

  it should "generate valid arpeggios" in {
    arpeggio(Chord("E")).head shouldBe List(0, 4, 7, 12)
  }

  it should "handle dim chord inversions" in {
    chords("2 x 1 x 1 x")
  }

  it should "name notes for pitch set chords" in {
    val c = Chord("{0 4 6 7}")
    val f = Operations.fingerings(c, 6, jazzVoicing = true)
    //println(f.head.show)
    Operations.notes(c)(f.head).show shouldBe "E x x G C F♯"
  }

  it should "find open C for mandolin" in {
    implicit val tuning: Tuning = Tuning("G D A E")
    val c = Chord("C/G")
    fingerings(c).map{_.show} should contain("0 10 10 0")
  }

  it should "find G for taro patch tuning" in {
    implicit val tuning: Tuning = Tuning("D G D G B D")
    val g = Chord("G")
    fingerings(g).map{_.show} should contain("0 0 0 0 0 0")
  }

  it should "find Dsus4 for DADGAD tuning" in {
    implicit val tuning: Tuning = Tuning("D A D G A D")
    val dsus4 = Chord("Dsus4")
    fingerings(dsus4).map{_.show} should contain("0 0 0 0 0 0")
  }
}
