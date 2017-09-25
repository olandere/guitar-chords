package chord.util

import scala.annotation.tailrec

class Modulo(m: Int) {

  @tailrec
  final def apply(n: Int): Int = if (n < 0) apply(n + m) else n % m

}
