package chord

sealed trait Quality {
  val invert: Quality
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
}

case object Minor extends Quality {
  override val invert: Quality = MajorInverval

  override def toString: String = "m"
}

case object Augmented extends Quality {
  override val invert: Quality = Diminished

  override def toString: String = "A"
}

case object DoublyAugmented extends Quality {
  override val invert: Quality = DoublyDiminished

  override def toString: String = "AA"
}

case object TripleAugmented extends Quality {
  override val invert: Quality = TripleDiminished

  override def toString: String = "AAA"
}

case object Diminished extends Quality {
  override val invert: Quality = Augmented

  override def toString: String = "d"
}

case object DoublyDiminished extends Quality {
  override val invert: Quality = DoublyAugmented

  override def toString: String = "dd"
}

case object TripleDiminished extends Quality {
  override val invert: Quality = TripleAugmented

  override def toString: String = "ddd"
}

case object Perfect extends Quality {
  override val invert: Quality = Perfect

  override def toString: String = "P"
}

case class Interval(quality: Quality, number: Int) {
  require (number > 0 && number < 9)

  private val s = Major(Note("C")).intervals

  def invert: Interval = new Interval(quality.invert, 9 - number)

  override def toString: String = s"$quality$number"

  def semitones = {
    s(number - 1) - (if (quality == Minor) 1 else 0)
  }

}

object Interval {
  val intervalMatcher = """(m|M|d|A|P|dd|AA|ddd|AAA)([1-8])""".r

  def apply(interval: String): Interval = {
    interval match {
      case intervalMatcher(q, n) => new Interval(Quality(q), n.toInt)
    }

  }
}
