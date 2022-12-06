package days

final case class Day6(input: String) extends Puzzle {

  private def findSequenceOfDistinctCharacters(numberCharacters: Int): Int =
    input
      .sliding(numberCharacters)
      .zipWithIndex
      .find { case (subString, _) => subString.distinct.size == subString.size }
      .fold(-1) { case (_, i) => i + numberCharacters }
  override def solve: String = findSequenceOfDistinctCharacters(4).toString()
  override def solve2: String = findSequenceOfDistinctCharacters(14).toString()
}
