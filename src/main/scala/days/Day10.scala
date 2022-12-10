package days
import cats.syntax.option._

final case class Day10(input: String) extends Puzzle {
  import Day10._

  private def readLine(s: String): Option[Op] =
    val noop = """^noop$""".r
    val addx = """^addx (-?\d+)""".r
    s match
      case noop()  => Noop.some
      case addx(x) => x.toIntOption.map(AddX.apply)
      case _       => None

  lazy val parsedInput: List[Op] =
    input
      .split("\n")
      .flatMap(readLine)
      .toList

  private def performInstruction(state: State, op: Op): State = op match
    case Noop =>
      State(state.xRegister, state.xRegister :: state.reversedHistoricValues)
    case AddX(value) =>
      val newXRegister = state.xRegister + value
      val newCycleValues = newXRegister :: state.xRegister :: state.reversedHistoricValues
      State(newXRegister, newCycleValues)

  lazy val finalState =
    // during cycle 1 the value is 1.
    // We pad with a 0 to have index 1 in the final list be the value
    // during cycle 1
    parsedInput.foldLeft(State(1, List(1, 0)))(performInstruction)

  private def overLapsWithSprite(pixel: Int, cycle: Int): Boolean =
    val spriteMidPosition = finalState.historicValues(cycle)

    val pixelsCoveredBySprite = List(spriteMidPosition, spriteMidPosition - 1, spriteMidPosition + 1)
    val overlaps = pixelsCoveredBySprite.exists(_ == pixel)
    overlaps
  override def solve: String =
    List(20, 60, 100, 140, 180, 220)
      .foldLeft(0) { case (acc, i) => finalState.historicValues(i) * i + acc }
      .toString()
  override def solve2: String =
    val pixels = for {
      row <- (0 until 6)
      pixel <- (0 until 40)
    } yield {
      val cycle = row * 40 + pixel + 1
      if (overLapsWithSprite(pixel, cycle)) "#" else "."
    }
    pixels.grouped(40).map(_.mkString("")).mkString("\n")
}

object Day10 {
  sealed trait Op
  case object Noop extends Op
  final case class AddX(value: Int) extends Op

  /** The state of the program during execution
    *
    * @param xRegister
    *   The current value of the xRegister
    * @param reversedHistoricValues
    *   The previous values of the xRegister as we are building the list as we calculate instructions
    */
  final case class State(xRegister: Int, reversedHistoricValues: List[Int]) {

    /** The historic values of the xRegister, i.e. for cycle *n* index *n* in this list denotes the value of the
      * xRegister during that cycle.
      */
    val historicValues = reversedHistoricValues.reverse
  }
}
