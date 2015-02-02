package chord

import org.scalatest.{ShouldMatchers, FlatSpec}
import Operations._
import scalaz._, syntax.show._

/**
 * Created by eolander on 12/28/14.
 */
class OperationsSpec extends FlatSpec with ShouldMatchers {

  "Fingerings" should "" in {

    assert(fingerings(Chord("E-9").asShell, 3).map{_.show.toString}.contains("x 7 5 7 7 x"))
    assert(fingerings(Chord("D-9").asShell, 3).map{_.show.toString}.contains("x 5 3 5 5 x"))
    assert(fingerings(Chord("FM7"), 4).map{_.show.toString}.contains("x x 3 2 1 0"))
    assert(fingerings(Chord("AM7"), 4).map{_.show.toString}.contains("x x 7 6 5 4"))
    assert(fingerings(Chord("BM7"), 4).map{_.show.toString}.contains("7 x 8 8 7 x"))
  }
}
