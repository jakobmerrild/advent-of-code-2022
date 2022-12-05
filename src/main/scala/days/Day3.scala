package days

final case class Day3(input: String) extends Puzzle {

  final case class Rucksack(items: String) {
    lazy val halfItems = items.size / 2
    lazy val firstHalf: Set[Char] = items.take(halfItems).toSet
    lazy val secondHalf: Set[Char] = items.drop(halfItems).toSet

    lazy val duplicate: Option[Char] = firstHalf.intersect(secondHalf).headOption
    lazy val itemSet: Set[Char] = firstHalf ++ secondHalf
  }

  private def itemPriority(item: Char): Int =
    if (item.isLower)
      item.toInt - 96 // lower case ascii char starts at 'a' = 97
    else
      item.toInt - 38 // upper case ascii char starts at 'A' = 65 (-38 sets value to 27)
  override def solve: String =
    input.split("\n").map(Rucksack.apply).map(_.duplicate.fold(0)(itemPriority)).sum.toString
  override def solve2: String =
    input
      .split("\n")
      .grouped(3)
      .map { ruckSacks =>
        ruckSacks
          .map(Rucksack.apply)
          .map(_.itemSet)
          .reduceOption { case (s1, s2) => s1.intersect(s2) }
          .flatMap(_.headOption)
          .fold(0)(itemPriority)
      }
      .sum
      .toString
}
