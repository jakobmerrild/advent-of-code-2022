package days

final case class Day9(input: String) extends Puzzle {
  import Day9._
  lazy val parsedInput: List[Move] = input.split("\n").flatMap(Move.parse).toList

  final case class State(head: Knot, tailPositions: Set[Coordinates])

  def recordMove(state: State, move: Move): State =
    (1 to move.steps).foldLeft(state) { case (acc, _) =>
      val newHead = move match
        case Left(_) =>
          acc.head.moveLeft
        case Right(_) =>
          acc.head.moveRight
        case Up(_) =>
          acc.head.moveUp
        case Down(_) =>
          acc.head.moveDown
      val newTailPosition = newHead.tailCoordinates

      State(newHead, acc.tailPositions + newTailPosition)
    }

  def solveImpl(initialKnot: Knot): Set[Coordinates] =
    parsedInput.foldLeft(State(initialKnot, Set.empty))(recordMove).tailPositions

  override def solve: String = {
    val initialState = Knot.buildRope(2)
    solveImpl(initialState).size.toString()
  }

  override def solve2: String = {
    val initialState = Knot.buildRope(10)
    solveImpl(initialState).size.toString()
  }
}

object Day9 {
  sealed trait Move {
    def steps: Int
  }
  final case class Left(steps: Int) extends Move
  final case class Right(steps: Int) extends Move
  final case class Up(steps: Int) extends Move
  final case class Down(steps: Int) extends Move

  object Move {
    def parse(s: String): Option[Move] =
      s.split(" ").toList match {
        case "L" :: steps :: Nil => steps.toIntOption.map(Left(_))
        case "R" :: steps :: Nil => steps.toIntOption.map(Right(_))
        case "U" :: steps :: Nil => steps.toIntOption.map(Up(_))
        case "D" :: steps :: Nil => steps.toIntOption.map(Down(_))
        case _                   => None
      }
  }

  final case class Coordinates(x: Int, y: Int) {
    def isTooFarAway(other: Coordinates): Boolean =
      Math.abs(x - other.x) > 1 || Math.abs(y - other.y) > 1
  }

  object Coordinates {
    val origin: Coordinates = Coordinates(0, 0)
  }

  final case class Knot(coordinates: Coordinates, next: Option[Knot]) {
    def moveLeft: Knot =
      val newCoordinates = (coordinates.copy(x = coordinates.x - 1))
      Knot(newCoordinates, next.map(_.move(newCoordinates)))
    def moveRight: Knot =
      val newCoordinates = coordinates.copy(x = coordinates.x + 1)
      Knot(newCoordinates, next.map(_.move(newCoordinates)))
    def moveUp: Knot =
      val newCoordinates = coordinates.copy(y = coordinates.y + 1)
      Knot(newCoordinates, next.map(_.move(newCoordinates)))
    def moveDown: Knot =
      val newCoordinates = coordinates.copy(y = coordinates.y - 1)
      Knot(newCoordinates, next.map(_.move(newCoordinates)))

    def tailCoordinates: Coordinates = next match
      case None        => this.coordinates
      case Some(value) => value.tailCoordinates

    private def move(movedCoordinates: Coordinates): Knot = {
      val newCoordinates =
        if (!coordinates.isTooFarAway(movedCoordinates)) coordinates
        else
          val xComparison = coordinates.x.compare(movedCoordinates.x)
          val yComparison = coordinates.y.compare(movedCoordinates.y)
          val xMove = if (xComparison == 0) 0 else -xComparison / Math.abs(xComparison)
          val yMove = if (yComparison == 0) 0 else -yComparison / Math.abs(yComparison)
          coordinates.copy(x = coordinates.x + xMove, y = coordinates.y + yMove)
      Knot(newCoordinates, next.map(_.move(newCoordinates)))
    }
  }

  object Knot {
    def buildRope(size: Int): Knot =
      (1 until size).foldLeft(Knot(Coordinates.origin, None)) { case (acc, _) => Knot(Coordinates.origin, Some(acc)) }
  }
}
