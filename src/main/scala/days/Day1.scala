package days

import cats.effect.ExitCode
import cats.effect.IO
import scala.math.Ordered

final case class Day1(input: String) extends Puzzle {
  final case class Bag(snacks: Iterable[String]) extends Ordered[Bag] {
    val totalCalories: Int = snacks.flatMap(_.toIntOption).sum
    def compare(that: Bag): Int = this.totalCalories - that.totalCalories
  }

  private lazy val bags: Array[Bag] =
    input.split("\n\n").map { bag => Bag(bag.split("\n")) }

  override def solve: String =
    bags.maxOption.map(_.totalCalories).getOrElse(0).toString

  override def solve2: String =
    bags.sorted.reverse.take(3).map(_.totalCalories).sum.toString
}
