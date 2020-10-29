package Assignment1

object Roman extends App {

  def toRoman(n: Int): String = {
    def toRomanHelper(n: Int, digit: List[(String, Int)]): String = digit match {
      case Nil => ""
      case h :: t => h._1 * (n / h._2) + toRomanHelper(n % h._2, t)
    }

    toRomanHelper(n, List(("M", 1000), ("CM", 900), ("D", 500), ("CD", 400), ("C", 100), ("XC", 90),
      ("L", 50), ("XL", 40), ("X", 10), ("IX", 9), ("V", 5), ("IV", 4), ("I", 1)))
  }
}
