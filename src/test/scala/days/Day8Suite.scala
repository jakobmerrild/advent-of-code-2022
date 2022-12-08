package days

class Day8Suite extends munit.FunSuite {
  val exampleInput =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin

  val day8 = Day8(exampleInput)

  test("parses example input correctly") {
    val actual = day8.parsedInput
    val row0 = List(
      Day8.Tree(0, 0, 3),
      Day8.Tree(0, 1, 0),
      Day8.Tree(0, 2, 3),
      Day8.Tree(0, 3, 7),
      Day8.Tree(0, 4, 3)
    )
    val row1 = List(
      Day8.Tree(1, 0, 2),
      Day8.Tree(1, 1, 5),
      Day8.Tree(1, 2, 5),
      Day8.Tree(1, 3, 1),
      Day8.Tree(1, 4, 2)
    )
    val row2 = List(
      Day8.Tree(2, 0, 6),
      Day8.Tree(2, 1, 5),
      Day8.Tree(2, 2, 3),
      Day8.Tree(2, 3, 3),
      Day8.Tree(2, 4, 2)
    )
    val row3 = List(
      Day8.Tree(3, 0, 3),
      Day8.Tree(3, 1, 3),
      Day8.Tree(3, 2, 5),
      Day8.Tree(3, 3, 4),
      Day8.Tree(3, 4, 9)
    )
    val row4 = List(
      Day8.Tree(4, 0, 3),
      Day8.Tree(4, 1, 5),
      Day8.Tree(4, 2, 3),
      Day8.Tree(4, 3, 9),
      Day8.Tree(4, 4, 0)
    )
    val expected = List(row0, row1, row2, row3, row4)

    assertEquals(actual, expected)
  }
  test("solves part 1 example data correctly") {
    val actual = day8.solve
    val expected = "21"

    assertEquals(actual, expected)
  }

  test("solves part 2 example data correctly") {
    val actual = day8.solve2
    val expected = "8"

    assertEquals(actual, expected)
  }

}
