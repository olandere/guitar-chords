package chord

import grizzled.slf4j.Logging

import scala.util.parsing.combinator.RegexParsers

sealed trait Quality extends Ordered[Quality] {
  val invert: Quality
  val semitoneAdjustment: Int

  override def compare(that: Quality): Int = this.semitoneAdjustment - that.semitoneAdjustment
}

case object Quality {
  def apply(q: String): Quality = {
    q match {
      case "m" => Minor
      case "M" => MajorInverval
      case "A" => Augmented
      case "AA" => DoublyAugmented
      case "AAA" => TripleAugmented
      case "d" => Diminished
      case "dd" => DoublyDiminished
      case "ddd" => TripleDiminished
      case "P" => Perfect
    }
  }
}

case object MajorInverval extends Quality {
  override val invert: Quality = Minor

  override def toString: String = "M"

  override val semitoneAdjustment = 0
}

case object Minor extends Quality {
  override val invert: Quality = MajorInverval

  override def toString: String = "m"

  override val semitoneAdjustment = -1
}

case object Augmented extends Quality {
  override val invert: Quality = Diminished

  override def toString: String = "A"

  override val semitoneAdjustment = 1
}

case object DoublyAugmented extends Quality {
  override val invert: Quality = DoublyDiminished

  override def toString: String = "AA"

  override val semitoneAdjustment = 2
}

case object TripleAugmented extends Quality {
  override val invert: Quality = TripleDiminished

  override def toString: String = "AAA"

  override val semitoneAdjustment = 3
}

case object Diminished extends Quality {
  override val invert: Quality = Augmented

  override def toString: String = "d"

  override val semitoneAdjustment = -2
}

case object DoublyDiminished extends Quality {
  override val invert: Quality = DoublyAugmented

  override def toString: String = "dd"

  override val semitoneAdjustment = -3
}

case object TripleDiminished extends Quality {
  override val invert: Quality = TripleAugmented

  override def toString: String = "ddd"

  override val semitoneAdjustment = -4
}

case object Perfect extends Quality {
  override val invert: Quality = Perfect

  override def toString: String = "P"

  override val semitoneAdjustment = 0
}

case object InvalidQuality extends Quality {
  override val invert: Quality = InvalidQuality

  override def toString: String = "INVALID"

  override val semitoneAdjustment = 0

}

case class Interval(quality: Quality, number: Int) extends Ordered[Interval] {
  require (number > 0 && number < 9)

  private val s = Major(Note("C")).intervals

  def invert: Interval = new Interval(quality.invert, 9 - number)

  override def toString: String = s"$quality$number"

  def semitones: Int = {
    val adj = s(number - 1) + quality.semitoneAdjustment //- (if (quality == Minor) 1 else 0)
    if (Set(0,4,5)(number) && quality < Perfect) adj + 1 else adj
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Interval => if (this.number == that.number) {
      this.quality == that.quality
    } else {
      this.semitones == that.semitones
    }
    case _ => false
  }

  override def hashCode(): Int = semitones

  override def compare(that: Interval): Int = {
    this.semitones - that.semitones
  }
}

object Interval {

  def apply(interval: String): Interval = {
    IntervalParser(interval)
  }
}

object InvalidInterval extends Interval(InvalidQuality, 1) {}

object IntervalParser extends RegexParsers with Logging {
  val perfect = "P" ~> """1|4|5""".r ^^ {n => new Interval(Perfect, n.toInt)}
  val majMin = """m|M""".r ~ """2|3|6|7""".r ^^ {case q~n => new Interval(Quality(q), n.toInt)}
  val augDim = """d{1,3}|A{1,3}""".r ~ """[1-8]""".r ^^ {case q~n => new Interval(Quality(q), n.toInt)}

  val interval = perfect | majMin | augDim

  def apply(input: String): Interval = {
    parseAll(interval, input) match {
      case Success(result, _) => result
      case failure: NoSuccess =>
        error(s"$input: ${failure.msg}")
        InvalidInterval
    }
  }
}
