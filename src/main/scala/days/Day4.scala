package days

final case class Day4(input: String) extends Puzzle {
  final case class Assignment(lower: Int, upper: Int) {
    def contains(other: Assignment): Boolean = {
      this.lower <= other.lower && this.upper >= other.upper
    }

    def overLaps(other: Assignment): Boolean = {
      this.lower <= other.upper && this.upper >= other.lower
    }
  }

  private def constructAssignment(s: String): Option[Assignment] =
    s.split("-").toList match {
      case x :: y :: Nil =>
        for {
          lower <- x.toIntOption
          upper <- y.toIntOption
        } yield Assignment(lower, upper)
      case _ => None
    }
  private def readLine(s: String): Option[(Assignment, Assignment)] = {
    s.split(",").toList match {
      case x :: y :: Nil =>
        for {
          first <- constructAssignment(x)
          second <- constructAssignment(y)
        } yield (first, second)

      case _ => None
    }
  }
  override def solve: Int =
    input
      .split("\n")
      .flatMap(readLine)
      .filter { case (first, second) => first.contains(second) || second.contains(first) }
      .size
  override def solve2: Int =
    input
      .split("\n")
      .flatMap(readLine)
      .filter { case (first, second) => first.overLaps(second) }
      .size
}
