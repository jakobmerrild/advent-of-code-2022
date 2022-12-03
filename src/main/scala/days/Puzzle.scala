package days

import cats.effect.ExitCode
import cats.effect.IO

trait Puzzle {
  def solve: Int
  def solve2: Int
}
