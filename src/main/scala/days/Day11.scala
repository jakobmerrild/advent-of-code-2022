package days

import cats.syntax.validated

final case class Day11(input: String) extends Puzzle {
  import Day11._

  val indexRegex = """^\s*Monkey (\d+):\s*$""".r
  val startingItemsRegex = """^\s*Starting items: (.*)$""".r
  val operationRegex = """^\s*Operation: new = ([^ ]+) (\+|\*) ([^ ]+)$""".r
  val testRegex = """^\s*Test: divisible by (\d+)$""".r
  val ifTrueRegex = """^\s*If true: throw to monkey (\d+)$""".r
  val ifFalseRegex = """^\s*If false: throw to monkey (\d+)$""".r

  private def readMonkey(s: String): Monkey = {
    val lines = s.split("\n")
    val index = lines(0) match
      case indexRegex(i) => i.toInt

    val startingItems = lines(1) match
      case startingItemsRegex(items) => items.split(",").map(_.trim.toLong).toList

    val operation = lines(2) match
      case operationRegex(lhs, op, rhs) =>
        op match
          case "+" =>
            (lhs, rhs) match
              case ("old", "old") => (x: Long) => x + x
              case ("old", i)     => (x: Long) => x + i.toLong
              case (i, "old")     => (x: Long) => i.toLong + x
          case "*" =>
            (lhs, rhs) match
              case ("old", "old") => (x: Long) => x * x
              case ("old", i)     => (x: Long) => x * i.toLong
              case (i, "old")     => (x: Long) => i.toLong * x
    val test = lines(3) match
      case testRegex(modulo) => (x: Long) => x % modulo.toLong == 0

    val ifTrue = lines(4) match
      case ifTrueRegex(toMonkey) => toMonkey.toInt

    val ifFalse = lines(5) match
      case ifFalseRegex(toMonkey) => toMonkey.toInt

    new Monkey(index, startingItems, operation, test, ifTrue, ifFalse)
  }

  lazy val parsedInput: Array[Monkey] =
    input.split("\n\n").map { s =>
      readMonkey(s)
    }

  private def monkeyTurn(
      state: State,
      monkeyIndex: Int,
      dropWorry: Long => Long,
      manipulateWorry: Long => Long
  ): State =
    val monkey = state.monkeys(monkeyIndex)
    val handleItems = monkey.items.foldLeft(state) { case (acc, i) =>
      val numberItemsInspected = acc.numberItemsInspected.getOrElse(monkeyIndex, 0L) + 1
      val increasedWorry = monkey.increaseWorry(i)
      val newValue = dropWorry(increasedWorry)
      val toMonkeyIndex = if (monkey.test(newValue)) monkey.ifTrue else monkey.ifFalse
      val toMonkey = acc.monkeys(toMonkeyIndex)

      acc.copy(
        numberItemsInspected = acc.numberItemsInspected + (monkeyIndex -> numberItemsInspected),
        monkeys = acc.monkeys + (toMonkeyIndex -> toMonkey.addItem(manipulateWorry(newValue)))
      )
    }
    val resetItems = monkey.resetItems
    handleItems.copy(
      monkeys = handleItems.monkeys + (monkeyIndex -> resetItems)
    )

  private def solveImpl(numberOfTurns: Int, dropWorry: Long => Long, manipulateWorry: Long => Long)(
      state: State
  ): State =
    if (numberOfTurns < state.turn) state
    else {
      val newState = state.monkeys.toList.sortBy(_._1).foldLeft(state) { case (state, (i, monkey)) =>
        monkeyTurn(state, i, dropWorry, manipulateWorry)
      }
      solveImpl(numberOfTurns, dropWorry, manipulateWorry)(newState.copy(turn = newState.turn + 1))
    }

  private lazy val initialState: State =
    val monkeyMap = parsedInput.map(m => (m.index -> m)).toMap
    State(monkeyMap, Map.empty, 1)

  override def solve: String =
    val finalState = solveImpl(20, _ / 3L, identity)(initialState)
    finalState.numberItemsInspected.values.toList.sorted.reverse.take(2).fold[Long](1)(_ * _).toString

  override def solve2: String =
    val commonMultiple = (11 * 2 * 5 * 17 * 19 * 7 * 3 * 13).toLong
    val finalState = solveImpl(10_000, identity, _ % commonMultiple)(initialState)
    finalState.numberItemsInspected.values.toList.sorted.reverse.take(2).fold[Long](1)(_ * _).toString

}

object Day11 {
  final case class Monkey(
      index: Int,
      items: List[Long],
      increaseWorry: Long => Long,
      test: Long => Boolean,
      ifTrue: Int,
      ifFalse: Int
  ) {
    var numberItemsInspected: Long = 0

    def addItem(item: Long): Monkey =
      this.copy(items = this.items :+ item)

    def resetItems: Monkey = this.copy(items = Nil)
  }

  final case class State(
      monkeys: Map[Int, Monkey],
      numberItemsInspected: Map[Int, Long],
      turn: Int
  )
}
