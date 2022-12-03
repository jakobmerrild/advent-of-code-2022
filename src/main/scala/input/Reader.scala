package input

import cats.effect.IO

object Reader {
  def getInputForDay(day: Int): IO[String] =
    IO.blocking(io.Source.fromResource(s"$day/input.txt").getLines().mkString("\n"))
}
