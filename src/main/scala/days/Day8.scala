package days

final case class Day8(input: String) extends Puzzle {
  import Day8._

  lazy val parsedInput: List[List[Tree]] = input
    .split("\n")
    .zipWithIndex
    .map { case (row, rowNo) =>
      row.zipWithIndex.flatMap { case (column, columnNo) =>
        column.toString().toIntOption.map(c => Tree(rowNo, columnNo, c))
      }.toList
    }
    .toList

  lazy val transposedInput = parsedInput.transpose

  private def addVisibleTreesSingleRow(acc: Set[Tree])(row: List[Tree]): Set[Tree] = {
    def checkIfVisible(set: Set[Tree], tallestTree: Int, tree: Tree) = {
      if (tree.height > tallestTree)
        (set + tree, tree.height)
      else (set, tallestTree)
    }

    val (fromLeft, _) = row.foldLeft((acc, -1)) { case ((set, tallestTree), tree) =>
      checkIfVisible(set, tallestTree, tree)
    }

    val (fromRight, _) = row.foldRight((fromLeft, -1)) { case (tree, (set, tallestTree)) =>
      checkIfVisible(set, tallestTree, tree)
    }
    fromRight
  }

  private def findVisibleTrees: Set[Tree] = {
    val byRows = parsedInput.foldLeft(Set.empty[Tree]) { case (acc, row) => addVisibleTreesSingleRow(acc)(row) }
    val byColumns = transposedInput.foldLeft(byRows) { case (acc, column) =>
      addVisibleTreesSingleRow(acc)(column)
    }
    byColumns
  }

  private def calculateScenicScore(t: Tree): Int = {
    def calculateScoreInDirection(direction: List[Tree]): Int = {
      // We need to include the tree that stops the takeWhile unless there are no trees
      val shorterThanInDirection = direction.takeWhile(_.shorterThan(t))
      val includeLastTree = if (shorterThanInDirection.size < direction.size) 1 else 0
      shorterThanInDirection.size + includeLastTree
    }
    val west = parsedInput(t.row).take(t.column).reverse
    val east = parsedInput(t.row).drop(t.column + 1)
    val north = transposedInput(t.column).take(t.row).reverse
    val south = transposedInput(t.column).drop(t.row + 1)

    val westScore = calculateScoreInDirection(west)
    val eastScore = calculateScoreInDirection(east)
    val northScore = calculateScoreInDirection(north)
    val southScore = calculateScoreInDirection(south)
    val result = westScore * eastScore * northScore * southScore
    result
  }

  override def solve: String = findVisibleTrees.size.toString()

  override def solve2: String = parsedInput.flatten.map(calculateScenicScore).maxOption.getOrElse(-1).toString()

}

object Day8 {
  final case class Tree(row: Int, column: Int, height: Int) {
    def shorterThan(t: Tree) = this.height < t.height
  }
}
