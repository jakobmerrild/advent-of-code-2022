package days

import cats.instances.boolean

final case class Day14(input: String) extends Puzzle {
  import Day14._

  private def readCoordinates(s: String): Coordinates =
    val values = s.trim.split(",")
    Coordinates(values(0).toInt, values(1).toInt)

  private def readRow(s: String): List[Line] =
    s.split("->")
      .map(readCoordinates)
      .sliding(2)
      .map { line =>
        Line(line(0), line(1))
      }
      .toList

  lazy val lines: List[Line] =
    input.split("\n").flatMap(readRow).toList

  lazy val rockCoordinates: Set[Coordinates] =
    lines.flatMap { case Line(from, to) =>
      // horizontal line
      if (from.x == to.x)
        if (from.y <= to.y)
          (from.y to to.y).map(Coordinates(from.x, _)).toSet
        else
          (to.y to from.y).map(Coordinates(from.x, _)).toSet
      // vertical line
      else if (from.x <= to.x)
        (from.x to to.x).map(Coordinates(_, from.y))
      else
        (to.x to from.x).map(Coordinates(_, from.y))
    }.toSet

  lazy val greatestYRock = rockCoordinates.map(_.y).max

  private def moveSandOneStep(blocked: Coordinates => Boolean)(currentPosition: Coordinates)(
      sand: Set[Coordinates]
  ): Coordinates =
    val directlyBelow = Coordinates(currentPosition.x, currentPosition.y + 1)
    val belowToLeft = Coordinates(currentPosition.x - 1, currentPosition.y + 1)
    val belowToRight = Coordinates(currentPosition.x + 1, currentPosition.y + 1)

    val blockedBelow = blocked(directlyBelow) || rockCoordinates.contains(directlyBelow) || sand.contains(directlyBelow)
    val blockedToLeft = blocked(belowToLeft) || rockCoordinates.contains(belowToLeft) || sand.contains(belowToLeft)
    val blockedToRight = blocked(belowToRight) || rockCoordinates.contains(belowToRight) || sand.contains(belowToRight)

    if (!blockedBelow)
      directlyBelow
    else if (!blockedToLeft)
      belowToLeft
    else if (!blockedToRight)
      belowToRight
    else currentPosition

  private def dropSand(
      stopWhenTrue: Coordinates => Boolean,
      blocked: Coordinates => Boolean
  )(startPosition: Coordinates, sand: Set[Coordinates]): Coordinates =
    val movedSand = moveSandOneStep(blocked)(startPosition)(sand)
    if (movedSand == startPosition)
      startPosition // Sand rest somewhere
    else if (stopWhenTrue(movedSand))
      movedSand // Sand dropping into abyss
    else
      dropSand(stopWhenTrue, blocked)(movedSand, sand) // Sand still falling

  private def simulate(stopWhenTrue: Coordinates => Boolean, blocked: Coordinates => Boolean)(
      sand: Set[Coordinates]
  ): Set[Coordinates] =
    val startPosition = Coordinates(500, 0)
    val finalCoordinates = dropSand(stopWhenTrue, blocked)(startPosition, sand)
    if (stopWhenTrue(finalCoordinates))
      sand // Falling into the abyss stop simulation
    else
      simulate(stopWhenTrue, blocked)(
        sand + finalCoordinates
      ) // Add final coordinates to set of sand and continue simulation

  override def solve: String =
    simulate(c => c.y > greatestYRock, _ => false)(Set.empty).size.toString()
  override def solve2: String =
    val finalState = simulate(_ == Coordinates(500, 0), c => c.y == greatestYRock + 2)(Set.empty)
    // Since we stop the simulation when we reach the coordinates (500, 0)
    // and we don't include the final position we need to add 1 to the size of the sand coordinates
    (finalState.size + 1).toString()
}

object Day14 {
  final case class Coordinates(x: Int, y: Int)
  final case class Line(from: Coordinates, to: Coordinates)
}
