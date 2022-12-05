package days

import cats.syntax.all._

final case class Day5(input: String) extends Puzzle {
  private case class Move(amount: Int, from: Int, to: Int)

  private lazy val initialState: Map[Int, List[String]] = {
    val stackRows = input.split("\n").takeWhile(_.nonEmpty).reverse.drop(1)
    val stacks = stackRows.map(_.grouped(4).map(_.replaceAll("[\\[\\]]", "").trim).toArray)
    stacks.transpose
      .map(_.toList.filter(_.nonEmpty))
      .zipWithIndex
      .map { case (items, i) => i + 1 -> items.reverse }
      .toMap
  }

  private def readMove(s: String): Option[Move] = {
    s.split(" ").flatMap(_.toIntOption).toList match
      case amount :: from :: to :: Nil => Move(amount, from, to).some
      case _                           => None
  }

  private lazy val instructions: List[Move] = input.split("\n").dropWhile(_.nonEmpty).drop(1).flatMap(readMove).toList

  private def performMove(reverse: Boolean)(state: Map[Int, List[String]], m: Move): Map[Int, List[String]] = {
    val fromStack = state.getOrElse(m.from, List.empty)
    val movedCrates = fromStack.take(m.amount)
    val newFromStack = fromStack.drop(m.amount)
    val toStack = state.getOrElse(m.to, List.empty)
    val newToStack =
      if (reverse)
        movedCrates.reverse ++ toStack
      else movedCrates ++ toStack
    (state + (m.from -> newFromStack)) + (m.to -> newToStack)
  }

  private def solveImpl(reverse: Boolean) = instructions
    .foldLeft(initialState)(performMove(reverse))
    .toList
    .sortBy { case (index, _) => index }
    .flatMap { case (_, stack) => stack.headOption }
    .mkString("")

  override def solve: String = solveImpl(true)

  override def solve2: String = solveImpl(false)
}
